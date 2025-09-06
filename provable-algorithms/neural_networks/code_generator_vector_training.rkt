#lang racket

(require "code_generator_core_training.rkt")
(provide train-lax-friedrichs-vector2-1d)

;; ----------------------------------------------------------------------------------------------------
;; Train a Lax–Friedrichs (Finite-Difference) Surrogate Solver for a 1D Coupled Vector System of 2 PDEs
;; ----------------------------------------------------------------------------------------------------
(define (train-lax-friedrichs-vector2-1d pde-system neural-net
                                         #:nx [nx 200]
                                         #:x0 [x0 0.0]
                                         #:x1 [x1 2.0]
                                         #:t-final [t-final 1.0]
                                         #:cfl [cfl 0.95]
                                         #:init-funcs [init-funcs (list
                                                                   `(cond
                                                                      [(< x 0.5) 3.0]
                                                                      [else 1.0])
                                                                   `(cond
                                                                      [(< x 0.5) 1.5]
                                                                      [else 0.0]))])
  "Generate C code that trains a surrogate solver for the 1D coupled vector system of 2 PDEs specified by `pde-system` using the Lax-Friedrichs finite-difference method,
   with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-funcs`: Racket expressions for the initial conditions, e.g. piecewise constant."

  (define name (hash-ref pde-system 'name))
  (define cons-exprs (hash-ref pde-system 'cons-exprs))
  (define flux-exprs (hash-ref pde-system 'flux-exprs))
  (define max-speed-exprs (hash-ref pde-system 'max-speed-exprs))
  (define parameters (hash-ref pde-system 'parameters))

  (define max-trains (hash-ref neural-net 'max-trains))
  (define width (hash-ref neural-net 'width))
  (define depth (hash-ref neural-net 'depth))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define flux-codes (map (lambda (flux-expr)
                            (convert-expr flux-expr)) flux-exprs))
  (define max-speed-codes (map (lambda (max-speed-expr)
                                 (convert-expr max-speed-expr)) max-speed-exprs))
  (define init-func-codes (map (lambda (init-func-expr)
                                 (convert-expr init-func-expr)) init-funcs))

  (define flux-ums (map (lambda (flux-code)
                          (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "um[0]")
                                           (list-ref cons-codes 1) "um[1]")) flux-codes))
  (define flux-uis (map (lambda (flux-code)
                          (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "ui[0]")
                                           (list-ref cons-codes 1) "ui[1]")) flux-codes))
  (define flux-ups (map (lambda (flux-code)
                          (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "up[0]")
                                           (list-ref cons-codes 1) "up[1]")) flux-codes))

  (define max-speed-locals (map (lambda (max-speed-code)
                                  (flux-substitute (flux-substitute max-speed-code (list-ref cons-codes 0) "u[(i * 2) + 0]")
                                                   (list-ref cons-codes 1) "u[(i * 2) + 1]")) max-speed-codes))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR TRAINING ON COUPLED VECTOR PDE SYSTEM: ~a
// Train a Lax–Friedrichs first-order finite-difference surrogate solver for a coupled vector system of 2 PDEs in 1D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include \"kann.h\"

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int nx = ~a;
  const double x0 = ~a;
  const double x1 = ~a;
  const double L = (x1 - x0);
  const double dx = L / nx;

  // Time-stepper setup.
  const double cfl = ~a;
  const double t_final = ~a;

  // Neural network hyperparameters.
  const double num_trains = ~a;
  const int nn_width = ~a;
  const int nn_depth = ~a;

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 2) * 2 * sizeof(double));
  double *un = (double*) malloc((nx + 2) * 2 * sizeof(double));

  // Arrays for storing other intermediate values.
  double *local_alpha = (double*) malloc(2 * sizeof(double));

  double *um = (double*) malloc(2 * sizeof(double));
  double *ui = (double*) malloc(2 * sizeof(double));
  double *up = (double*) malloc(2 * sizeof(double));

  double *f_um = (double*) malloc(2 * sizeof(double));
  double *f_ui = (double*) malloc(2 * sizeof(double));
  double *f_up = (double*) malloc(2 * sizeof(double));

  double *fluxL = (double*) malloc(2 * sizeof(double));
  double *fluxR = (double*) malloc(2 * sizeof(double));

  // Arrays for storing training data.
  float ***input_data = (float***) malloc(2 * sizeof(float**));
  float ***output_data = (float***) malloc(2 * sizeof(float**));
  
  for (int i = 0; i < 2; i++) {
    input_data[i] = (float**) malloc(nx * num_trains * sizeof(float*));
    output_data[i] = (float**) malloc(nx * num_trains * sizeof(float*));
  }

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 2) + 1] = ~a; // init-funcs[1] in C.
  }

  // Initialize neural network architecture.
  kad_node_t **t_net = (kad_node_t**) malloc(2 * sizeof(kad_node_t*));
  kann_t **ann = (kann_t**) malloc(2 * sizeof(kann_t*));

  for (int i = 0; i < 2; i++) {
    t_net[i] = kann_layer_input(2);
  
    for (int j = 0; j < nn_depth; j++) {
      t_net[i] = kann_layer_dense(t_net[i], nn_width);
      t_net[i] = kad_tanh(t_net[i]);
    }

    t_net[i] = kann_layer_cost(t_net[i], 1, KANN_C_MSE);
    ann[i] = kann_new(t_net[i], 0);
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      local_alpha[0] = ~a; // max-speed-exprs[0] in C.
      local_alpha[1] = ~a; // max-speed-exprs[1] in C.
      
      for (int j = 0; j < 2; j++) {
        if (local_alpha[j] > alpha) {
          alpha = local_alpha[j];
        }
      }
    }

    // Avoid division by zero.
    if (alpha < 1e-14) {
      alpha = 1e-14;
    }

    // Compute stable time step from alpha.
    double dt = cfl * dx / alpha;

    // If stepping beyond t_final, adjust dt accordingly.
    if (t + dt > t_final) {
      dt = t_final - t;
    }

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable vector.
    for (int i = 1; i <= nx; i++) {
      for (int j = 0; j < 2; j++) {
        um[j] = u[((i - 1) * 2) + j];
        ui[j] = u[(i * 2) + j];
        up[j] = u[((i + 1) * 2) + j];
      }

      // Evaluate flux vector for each value of the conserved variable vector.
      f_um[0] = ~a;
      f_um[1] = ~a; // F(U_{i - 1}).
      
      f_ui[0] = ~a;
      f_ui[1] = ~a; // F(U_i).
      
      f_up[0] = ~a;
      f_up[1] = ~a; // F(U_{i + 1}).

      // Left interface flux: F_{i - 1/2} = 0.5 * (F(U_{i - 1}) + F(U_i)) - 0.5 * alpha * (U_i - U_{i - 1}).
      for (int j = 0; j < 2; j++) {
        fluxL[j] = 0.5 * (f_um[j] + f_ui[j]) - 0.5 * alpha * (ui[j] - um[j]);
      }

      // Right interface flux: F_{i + 1/2} = 0.5 * (F(U_{i + 1}) + F(U_i)) - 0.5 * alpha * (U_{i + 1} - U_i).
      for (int j = 0; j < 2; j++) {
        fluxR[j] = 0.5 * (f_ui[j] + f_up[j]) - 0.5 * alpha * (up[j] - ui[j]);
      }

      // Update the conserved variable vector.
      for (int j = 0; j < 2; j++) {
        un[(i * 2) + j] = ui[j] - (dt / dx) * (fluxR[j] - fluxL[j]);
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j < 2; j++) {
        u[(i * 2) + j] = un[(i * 2) + j];
      }
    }

    // Apply simple boundary conditions (transmissive).
    for (int j = 0; j < 2; j++) {
      u[(0 * 2) + j] = u[(1 * 2) + j];
      u[((nx + 1) * 2) + j] = u[(nx * 2) + j];
    }

    // Accumulate to training data.
    if (n < num_trains) {
      for (int i = 1; i <= nx; i++) {
        double x = x0 + (i - 0.5) * dx;

        for (int j = 0; j < 2; j++) {
          input_data[j][(n * nx) + (i - 1)] = (float*) malloc(2 * sizeof(float));
          output_data[j][(n * nx) + (i - 1)] = (float*) malloc(sizeof(float));
      
          input_data[j][(n * nx) + (i - 1)][0] = t;
          input_data[j][(n * nx) + (i - 1)][1] = x;
          output_data[j][(n * nx) + (i - 1)][0] = u[(i * 2) + j];
        }
      }
    }

    // Output solution to disk.
    for (int j = 0; j < 2; j++) {
      const char *fmt = \"%s_output_%d_%d.csv\";
      int sz = snprintf(0, 0, fmt, \"~a\", j, n);
      char file_nm[sz + 1];
      snprintf(file_nm, sizeof file_nm, fmt, \"~a\", j, n);
    
      FILE *fptr = fopen(file_nm, \"w\");
      if (fptr != NULL) {
        for (int i = 1; i <= nx; i++) {
          double x = x0 + (i - 0.5) * dx;
          fprintf(fptr, \"%f, %f\\n\", x, u[(i * 2) + j]);
        }

        fclose(fptr);
      }
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  // Train neural network.
  for (int i = 0; i < 2; i++) {
    kann_train_fnn1(ann[i], 0.0001f, 64, 50, 10, 0.1f, n * nx, input_data[i], output_data[i]);
  }

  // Output neural network to disk.
  for (int i = 0; i < 2; i++) {
    const char *fmt = \"%s_%d_neural_net.dat\";
    int sz = snprintf(0, 0, fmt, \"~a\", i);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", i);
  
    kann_save(file_nm, ann[i]);
  }

  free(u);
  free(un);

  free(local_alpha);
  
  free(um);
  free(ui);
  free(up);

  free(f_um);
  free(f_ui);
  free(f_up);

  free(fluxL);
  free(fluxR);

  for (int i = 0; i < 2; i++) {
    kann_delete(ann[i]);
  }
  free(ann);
  free(t_net);

  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < nx * num_trains; j++) {
      free(input_data[i][j]);
      free(output_data[i][j]);
    }

    free(input_data[i]);
    free(output_data[i]);
  }

  free(input_data);
  free(output_data);
  
  return 0;
}
"
           ;; PDE name for code comments.
           name
           ;; Additional PDE parameters (e.g. a = 1.0 for linear advection).
           parameter-code
           ;; Number of cells.
           nx
           ;; Left boundary.
           x0
           ;; Right boundary.
           x1
           ;; CFL coefficient.
           cfl
           ;; Final time.
           t-final
           ;; Maximum number of time-steps to train on.
           max-trains
           ;; Neural network width.
           width
           ;; Neural network depth.
           depth
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           ;; Left flux vector F(u_{i - 1}).
           (list-ref flux-ums 0)
           (list-ref flux-ums 1)
           ;; Middle flux vector F(u_i).
           (list-ref flux-uis 0)
           (list-ref flux-uis 1)
           ;; Right flux vector F(u_{i + 1}).
           (list-ref flux-ups 0)
           (list-ref flux-ups 1)
           ;; PDE name for file output.
           name
           name
           ;; PDE name for neural network output.
           name
           name
           ))
  code)