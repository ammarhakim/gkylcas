#lang racket

(provide convert-expr
         remove-bracketed-expressions
         remove-bracketed-expressions-from-file
         flux-substitute
         train-lax-friedrichs-scalar-1d
         train-lax-friedrichs-scalar-1d-second-order
         train-lax-friedrichs-scalar-2d)

;; Lightweight converter from Racket expressions (expr) into strings representing equivalent C code.
(define (convert-expr expr)
  (match expr    
    ;; If expr is a symbol, then convert it directly to a string.
    [(? symbol? symb) (symbol->string symb)]

    ;; If expr is a numerical constant, then convert it directly to a string.
    [(? number? num) (number->string num)]

    ;; If expr is a sum of the form (+ expr1 expr2 ...), then convert it to "(expr1 + expr2 + ...)" in C.
    [`(+ . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " + ") ")"))]
    ;; Likewise for differences.
    [`(- . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " - ") ")"))]

    ;; If expr is a product of the form (* expr1 expr2 ...), then convert it to "(expr1 * expr2 * ...)" in C.
    [`(* . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " * ") ")"))]
    ;; Likewise for quotients.
    [`(/ . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " / ") ")"))]

    ;; If expr is an absolute value of the form (abs expr1), then convert it to "fabs(expr1)" in C.
    [`(abs ,arg)
     (format "fabs(~a)" (convert-expr arg))]

    ;; If expr is a square root of the form (sqrt expr1), then convert it to "sqrt(expr1)" in C.
    [`(sqrt ,arg)
     (format "sqrt(~a)" (convert-expr arg))]

    ;; If expr is a maximum of the form (max expr1 expr2), then convert it to "fmax(expr1, expr2)" in C.
    [`(max ,arg1 ,arg2)
     (format "fmax(~a, ~a)" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a maximum of the form (max expr1 expr2 expr2), then convert it to "fmax(expr1, expr2, expr3)" in C.
    [`(max ,arg1 ,arg2 ,arg3)
     (format "fmax3(~a, ~a, ~a)" (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    ;; If expr is a minimum of the form (max expr1 expr2), then convert it to "fmin(expr1, expr2)" in C.
    [`(min ,arg1 ,arg2)
     (format "fmin(~a, ~a)" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a minimum of the form (max expr1 expr2 expr2), then convert it to "fmin(expr1, expr2, expr3)" in C.
    [`(min ,arg1 ,arg2 ,arg3)
     (format "fmin3(~a, ~a, ~a)" (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    ;; If expr is a variable assignment of the form (define expr1 expr2), then convert it to "expr1 = expr2" in C.
    [`(define ,arg1 ,arg2)
     (format "~a = ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a strict comparison of the form (< expr1 expr2), then convert it to "expr1 < expr2" in C.
    [`(< ,arg1 ,arg2)
     (format "~a < ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a comparison of the form (<= expr1 expr2), then convert it to "expr1 <= expr2" in C.
    [`(<= ,arg1 ,arg2)
     (format "~a <= ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a strict comparison of the form (> expr1 expr2), then convert it to "expr1 > expr2" in C.
    [`(> ,arg1 ,arg2)
     (format "~a > ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a comparison of the form (>= expr1 expr2), then convert it to "expr1 >= expr2" in C.
    [`(>= ,arg1 ,arg2)
     (format "~a >= ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is an equality comparison of the form (equal? expr1 expr2), then convert it to "expr1 == expr2" in C.
    [`(equal? ,arg1 ,arg2)
     (format "~a == ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a conditional of the form [(cond [cond1 expr1] [else expr2])], then convert it to the ternary operator "(cond1) ? expr1 : expr2" in C.
    [`(cond
        [,cond1 ,expr1]
        [else ,expr2])
     (format "(~a) ? ~a : ~a" (convert-expr cond1) (convert-expr expr1) (convert-expr expr2))]))

;; A simple boilerplate function for removing bracketed expressions from strings.
(define (remove-bracketed-expressions str)
  (regexp-replace* #rx"\\[.*?\\]" str ""))

;; A simple boilerplate function for removing bracketed expressions from files.
(define (remove-bracketed-expressions-from-file output-file)
  (define content
    (with-input-from-file output-file
      (lambda ()
        (port->string (current-input-port)))))
  (define cleaned
    (remove-bracketed-expressions content))
  (with-output-to-file output-file #:exists 'replace
    (lambda ()
      (display cleaned))))

(define (flux-substitute flux-expr cons-expr var-name)
  (string-replace flux-expr cons-expr var-name))

;; -------------------------------------------------------------------------------
;; Train a Lax–Friedrichs (Finite-Difference) Surrogate Solver for a 1D Scalar PDE
;; -------------------------------------------------------------------------------
(define (train-lax-friedrichs-scalar-1d pde neural-net
                                        #:nx [nx 200]
                                        #:x0 [x0 0.0]
                                        #:x1 [x1 2.0]
                                        #:t-final [t-final 1.0]
                                        #:cfl [cfl 0.95]
                                        #:init-func [init-func `(cond
                                                                  [(< x 1.0) 1.0]
                                                                  [else 0.0])])
  "Generate C code that trains a surrogate solver for the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method,
   with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define cons-expr (hash-ref pde 'cons-expr))
  (define flux-expr (hash-ref pde 'flux-expr))
  (define max-speed-expr (hash-ref pde 'max-speed-expr))
  (define parameters (hash-ref pde 'parameters))

  (define max-trains (hash-ref neural-net 'max-trains))
  (define width (hash-ref neural-net 'width))
  (define depth (hash-ref neural-net 'depth))

  (define cons-code (convert-expr cons-expr))
  (define flux-code (convert-expr flux-expr))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))

  (define flux-um (flux-substitute flux-code cons-code "um"))
  (define flux-ui (flux-substitute flux-code cons-code "ui"))
  (define flux-up (flux-substitute flux-code cons-code "up"))

  (define max-speed-local (flux-substitute max-speed-code cons-code "u[i]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR TRAINING ON SCALAR PDE: ~a
// Train a Lax–Friedrichs first-order finite-difference surrogate solver for a scalar PDE in 1D.

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
  double *u = (double*) malloc((nx + 2) * sizeof(double));
  double *un = (double*) malloc((nx + 2) * sizeof(double));

  // Arrays for storing training data.
  float **input_data = (float**) malloc(nx * num_trains * sizeof(float*));
  float **output_data = (float**) malloc(nx * num_trains * sizeof(float*));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  // Initialize neural network architecture.
  kad_node_t *t_net;
  kann_t *ann;
  t_net = kann_layer_input(2);
  
  for (int i = 0; i < nn_depth; i++) {
    t_net = kann_layer_dense(t_net, nn_width);
    t_net = kad_tanh(t_net);
  }

  t_net = kann_layer_cost(t_net, 1, KANN_C_MSE);
  ann = kann_new(t_net, 0);

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      double local_alpha = ~a; // max-speed-expr in C.
      
      if (local_alpha > alpha) {
        alpha = local_alpha;
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

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable.
    for (int i = 1; i <= nx; i++) {
      double um = u[i - 1];
      double ui = u[i];
      double up = u[i + 1];

      // Evaluate flux for each value of the conserved variable.
      double f_um = ~a; // f(u_{i - 1}).
      double f_ui = ~a; // f(u_i).
      double f_up = ~a; // f(u_{i + 1}).

      // Left interface flux: F_{i - 1/2} = 0.5 * (f(u_{i - 1}) + f(u_i)) - 0.5 * alpha * (u_i - u_{i - 1}).
      double fluxL = 0.5 * (f_um + f_ui) - 0.5 * alpha * (ui - um);

      // Right interface flux: F_{i + 1/2} = 0.5 * (f(u_{i + 1}) + f(u_i)) - 0.5 * alpha * (u_{i + 1} - u_i).
      double fluxR = 0.5 * (f_ui + f_up) - 0.5 * alpha * (up - ui);

      // Update the conserved variable.
      un[i] = ui - (dt / dx) * (fluxR - fluxL);
    }

    // Copy un -> u (updated conserved variables to new conserved variables).
    for (int i = 0; i <= nx + 1; i++) {
      u[i] = un[i];
    }

    // Apply simple boundary conditions (transmissive).
    u[0] = u[1];
    u[nx + 1] = u[nx];

    // Accumulate to training data.
    if (n < num_trains) {
      for (int i = 1; i <= nx; i++) {
        double x = x0 + (i - 0.5) * dx;

        input_data[(n * nx) + (i - 1)] = (float*) malloc(2 * sizeof(float));
        output_data[(n * nx) + (i - 1)] = (float*) malloc(sizeof(float));
      
        input_data[(n * nx) + (i - 1)][0] = t;
        input_data[(n * nx) + (i - 1)][1] = x;
        output_data[(n * nx) + (i - 1)][0] = u[i];
      }
    }

    // Output solution to disk.
    const char *fmt = \"%s_output_%d.csv\";
    int sz = snprintf(0, 0, fmt, \"~a\", n);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", n);
    
    FILE *fptr = fopen(file_nm, \"w\");
    if (fptr != NULL) {
      for (int i = 1; i <= nx; i++) {
        double x = x0 + (i - 0.5) * dx;
        fprintf(fptr, \"%f, %f\\n\", x, u[i]);
      }

      fclose(fptr);
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  // Train neural network.
  kann_train_fnn1(ann, 0.0001f, 64, 50, 10, 0.1f, n * nx, input_data, output_data);

  // Output neural network to disk.
  const char *fmt = \"%s_neural_net.dat\";
  int sz = snprintf(0, 0, fmt, \"~a\");
  char file_nm[sz + 1];
  snprintf(file_nm, sizeof file_nm, fmt, \"~a\");
  
  kann_save(file_nm, ann);

  free(u);
  free(un);

  kann_delete(ann);
  
  for (int i = 0; i < nx * num_trains; i++) {
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
           ;; Initial condition expression (e.g. (x < 1.0) ? 1.0 : 0.0)).
           init-func-code
           ;; Expression for local wave-speed estimate.
           max-speed-local
           ;; Left flux f(u_{i - 1}).
           flux-um
           ;; Middle flux f(u_i).
           flux-ui
           ;; Right flux f(u_{i + 1}).
           flux-up
           ;; PDE name for file output.
           name
           name
           ;; PDE name for neural network output.
           name
           name
           ))
  code)

;; ----------------------------------------------------------------------------------------------------------------------
;; Train a Lax–Friedrichs (Finite-Difference) Surrogate Solver for a 1D Scalar PDE with a Second-Order Flux Extrapolation
;; ----------------------------------------------------------------------------------------------------------------------
(define (train-lax-friedrichs-scalar-1d-second-order pde limiter neural-net
                                                     #:nx [nx 200]
                                                     #:x0 [x0 0.0]
                                                     #:x1 [x1 2.0]
                                                     #:t-final [t-final 1.0]
                                                     #:cfl [cfl 0.95]
                                                     #:init-func [init-func `(cond
                                                                               [(< x 1.0) 1.0]
                                                                               [else 0.0])])
  "Generate C code that trains a surrogate solver for the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method with a second-order flux extrapolation
   using the flux limiter `limiter`, with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define cons-expr (hash-ref pde 'cons-expr))
  (define flux-expr (hash-ref pde 'flux-expr))
  (define max-speed-expr (hash-ref pde 'max-speed-expr))
  (define parameters (hash-ref pde 'parameters))

  (define limiter-name (hash-ref limiter 'name))
  (define limiter-expr (hash-ref limiter 'limiter-expr))
  (define limiter-ratio (hash-ref limiter 'limiter-ratio))

  (define limiter-code (convert-expr limiter-expr))
  (define limiter-ratio-code (convert-expr limiter-ratio))

  (define max-trains (hash-ref neural-net 'max-trains))
  (define width (hash-ref neural-net 'width))
  (define depth (hash-ref neural-net 'depth))

  (define cons-code (convert-expr cons-expr))
  (define flux-code (convert-expr flux-expr))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))

  (define limiter-r (flux-substitute limiter-code limiter-ratio-code "r"))

  (define flux-umL (flux-substitute flux-code cons-code "umL"))
  (define flux-umR (flux-substitute flux-code cons-code "umR"))
  (define flux-uiL (flux-substitute flux-code cons-code "uiL"))
  (define flux-uiR (flux-substitute flux-code cons-code "uiR"))
  (define flux-upL (flux-substitute flux-code cons-code "upL"))
  (define flux-upR (flux-substitute flux-code cons-code "upR"))

  (define flux-umR-evol (flux-substitute flux-code cons-code "umR_evol"))
  (define flux-uiL-evol (flux-substitute flux-code cons-code "uiL_evol"))
  (define flux-uiR-evol (flux-substitute flux-code cons-code "uiR_evol"))
  (define flux-upL-evol (flux-substitute flux-code cons-code "upL_evol"))

  (define max-speed-local (flux-substitute max-speed-code cons-code "u[i]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR TRAINING ON SCALAR PDE: ~a
// FLUX LIMITER: ~a
// Train a Lax–Friedrichs first-order finite-difference surrogate solver for a scalar PDE in 1D, with a second-order flux extrapolation.

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

  // Array for storing slopes.
  double *slope = (double*) malloc((nx + 4) * sizeof(double));

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 4) * sizeof(double));
  double *un = (double*) malloc((nx + 4) * sizeof(double));

  // Arrays for storing training data.
  float **input_data = (float**) malloc(nx * num_trains * sizeof(float*));
  float **output_data = (float**) malloc(nx * num_trains * sizeof(float*));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 3; i++) {
    double x = x0 + (i - 1.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  // Initialize neural network architecture.
  kad_node_t *t_net;
  kann_t *ann;
  t_net = kann_layer_input(2);
  
  for (int i = 0; i < nn_depth; i++) {
    t_net = kann_layer_dense(t_net, nn_width);
    t_net = kad_tanh(t_net);
  }

  t_net = kann_layer_cost(t_net, 1, KANN_C_MSE);
  ann = kann_new(t_net, 0);

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 2; i <= nx + 1; i++) {
      double local_alpha = ~a; // max-speed-expr in C.
      
      if (local_alpha > alpha) {
        alpha = local_alpha;
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

    // Compute appropriately flux-limited slopes within each cell.
    for (int i = 1; i <= nx + 2; i++) {
      double r = (u[i] - u[i - 1]) / (u[i + 1] - u[i]);
      double limiter = ~a; // limiter-r in C.

      slope[i] = limiter * (0.5 * ((u[i] - u[i - 1]) + (u[i + 1] - u[i])));
    }

    // Compute fluxes with Lax-Friedrichs approximation (with a second-order flux extrapolation) and update the conserved variable.
    for (int i = 2; i <= nx + 1; i++) {
      // Extrapolate boundary states.
      double umL = u[i - 1] - (0.5 * slope[i - 1]);
      double umR = u[i - 1] + (0.5 * slope[i - 1]);

      double uiL = u[i] - (0.5 * slope[i]);
      double uiR = u[i] + (0.5 * slope[i]);

      double upL = u[i + 1] - (0.5 * slope[i + 1]);
      double upR = u[i + 1] + (0.5 * slope[i + 1]);

      // Evaluate flux for each extrapolated boundary state.
      double f_umL = ~a;
      double f_umR = ~a;

      double f_uiL = ~a;
      double f_uiR = ~a;

      double f_upL = ~a;
      double f_upR = ~a;

      // Evolve each extrapolated boundary state.
      double umR_evol = umR + ((dt / (2.0 * dx)) * (f_umL - f_umR));

      double uiL_evol = uiL + ((dt / (2.0 * dx)) * (f_uiL - f_uiR));
      double uiR_evol = uiR + ((dt / (2.0 * dx)) * (f_uiL - f_uiR));

      double upL_evol = upL + ((dt / (2.0 * dx)) * (f_upL - f_upR));

      // Evaluate flux for each value of the (evolved) conserved variable.
      double f_umR_evol = ~a;
      double f_uiL_evol = ~a;

      double f_uiR_evol = ~a;
      double f_upL_evol = ~a;

      // Left interface flux: F_{i - 1/2} = 0.5 * (f(u_{i - 1, R+}) + f(u_{i, L+})) - 0.5 * alpha * (u_{i, L+} - u_{i - 1, R+}).
      double fluxL = 0.5 * (f_umR_evol + f_uiL_evol) - 0.5 * alpha * (uiL_evol - umR_evol);

      // Right interface flux: F_{i + 1/2} = 0.5 * (f(u_{i + 1, L+}) + f(u_{i, R+})) - 0.5 * alpha * (u_{i + 1, L+} - u_{i, R+}).
      double fluxR = 0.5 * (f_uiR_evol + f_upL_evol) - 0.5 * alpha * (upL_evol - uiR_evol);

      // Update the conserved variable.
      un[i] = u[i] - (dt / dx) * (fluxR - fluxL);
    }

    // Copy un -> u (updated conserved variables to new conserved variables).
    for (int i = 0; i <= nx + 3; i++) {
      u[i] = un[i];
    }

    // Apply simple boundary conditions (transmissive).
    u[0] = u[2];
    u[1] = u[2];
    u[nx + 2] = u[nx + 1];
    u[nx + 3] = u[nx + 1];

    // Accumulate to training data.
    if (n < num_trains) {
      for (int i = 2; i <= nx + 1; i++) {
        double x = x0 + (i - 1.5) * dx;

        input_data[(n * nx) + (i - 2)] = (float*) malloc(2 * sizeof(float));
        output_data[(n * nx) + (i - 2)] = (float*) malloc(sizeof(float));
      
        input_data[(n * nx) + (i - 2)][0] = t;
        input_data[(n * nx) + (i - 2)][1] = x;
        output_data[(n * nx) + (i - 2)][0] = u[i];
      }
    }

    // Output solution to disk.
    const char *fmt = \"%s_output_%d.csv\";
    int sz = snprintf(0, 0, fmt, \"~a\", n);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", n);
    
    FILE *fptr = fopen(file_nm, \"w\");
    if (fptr != NULL) {
      for (int i = 2; i <= nx + 1; i++) {
        double x = x0 + (i - 1.5) * dx;
        fprintf(fptr, \"%f, %f\\n\", x, u[i]);
      }

      fclose(fptr);
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  // Train neural network.
  kann_train_fnn1(ann, 0.0001f, 64, 50, 10, 0.1f, n * nx, input_data, output_data);

  // Output neural network to disk.
  const char *fmt = \"%s_neural_net.dat\";
  int sz = snprintf(0, 0, fmt, \"~a\");
  char file_nm[sz + 1];
  snprintf(file_nm, sizeof file_nm, fmt, \"~a\");
  
  kann_save(file_nm, ann);

  free(u);
  free(un);

  kann_delete(ann);
  
  for (int i = 0; i < nx * num_trains; i++) {
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
           ;; Flux limiter name for code comments.
           limiter-name
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
           ;; Initial condition expression (e.g. (x < 1.0) ? 1.0 : 0.0)).
           init-func-code
           ;; Expression for local wave-speed estimate.
           max-speed-local
           ;; Expression for flux limiter function.
           limiter-r
           ;; Left negative flux f(u_{i - 1, L}).
           flux-umL
           ;; Right negative flux f(u_{i - 1, R}).
           flux-umR
           ;; Left central flux f(u_{i, L}).
           flux-uiL
           ;; Right central flux f(u_{i, R}).
           flux-uiR
           ;; Left positive flux f(u_{i + 1, L}).
           flux-upL
           ;; Right positive flux f(u_{i + 1, R}).
           flux-upR
           ;; Evolved right negative flux f(u_{i - 1, R+}).
           flux-umR-evol
           ;; Evolved left central flux f(u_{i, L+}).
           flux-uiL-evol
           ;; Evolved right central flux f(u_{i, R+}).
           flux-uiR-evol
           ;; Evolved left positive flux f(u_{i + 1, L+}).
           flux-upL-evol
           ;; PDE name for file output.
           name
           name
           ;; PDE name for neural network output.
           name
           name
           ))
  code)

;; -------------------------------------------------------------------------------
;; Train a Lax–Friedrichs (Finite-Difference) Surrogate Solver for a 2D Scalar PDE
;; -------------------------------------------------------------------------------
(define (train-lax-friedrichs-scalar-2d pde neural-net
                                        #:nx [nx 200]
                                        #:ny [ny 200]
                                        #:x0 [x0 0.0]
                                        #:x1 [x1 2.0]
                                        #:y0 [y0 0.0]
                                        #:y1 [y1 2.0]
                                        #:t-final [t-final 1.0]
                                        #:cfl [cfl 0.95]
                                        #:init-func [init-func `(cond
                                                                  [(< (+ (* (- x 1.0) (- x 1.0)) (* (- y 1.0) (- y 1.0))) 0.5) 1.0]
                                                                  [else 0.0])])
  "Generate C code that trains a surrogate solver for the 2D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method,
   with neural network architecture `neural-net`.
  - `nx`, `ny` : Number of spatial cells in each coordinate direction.
  - `x0`, `x1`, `y0`, `y1` : Domain boundaries in each coordinate direction.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define cons-expr (hash-ref pde 'cons-expr))
  (define flux-expr-x (hash-ref pde 'flux-expr-x))
  (define flux-expr-y (hash-ref pde 'flux-expr-y))
  (define max-speed-expr-x (hash-ref pde 'max-speed-expr-x))
  (define max-speed-expr-y (hash-ref pde 'max-speed-expr-y))
  (define parameters (hash-ref pde 'parameters))

  (define max-trains (hash-ref neural-net 'max-trains))
  (define width (hash-ref neural-net 'width))
  (define depth (hash-ref neural-net 'depth))

  (define cons-code (convert-expr cons-expr))
  (define flux-code-x (convert-expr flux-expr-x))
  (define flux-code-y (convert-expr flux-expr-y))
  (define max-speed-code-x (convert-expr max-speed-expr-x))
  (define max-speed-code-y (convert-expr max-speed-expr-y))
  (define init-func-code (convert-expr init-func))

  (define flux-um-x (flux-substitute flux-code-x cons-code "um_x"))
  (define flux-ui-x (flux-substitute flux-code-x cons-code "ui_x"))
  (define flux-up-x (flux-substitute flux-code-x cons-code "up_x"))

  (define flux-um-y (flux-substitute flux-code-y cons-code "um_y"))
  (define flux-ui-y (flux-substitute flux-code-y cons-code "ui_y"))
  (define flux-up-y (flux-substitute flux-code-y cons-code "up_y"))

  (define max-speed-local-x (flux-substitute max-speed-code-x cons-code "u[i][j]"))
  (define max-speed-local-y (flux-substitute max-speed-code-y cons-code "u[i][j]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR TRAINING ON SCALAR PDE: ~a
// Train a Lax–Friedrichs first-order finite-difference surrogate solver for a scalar PDE in 2D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include \"kann.h\"

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int nx = ~a;
  const int ny = ~a;
  const double x0 = ~a;
  const double x1 = ~a;
  const double y0 = ~a;
  const double y1 = ~a;
  const double Lx = (x1 - x0);
  const double Ly = (y1 - y0);
  const double dx = Lx / nx;
  const double dy = Ly / ny;

  // Time-stepper setup.
  const double cfl = ~a;
  const double t_final = ~a;

  // Neural network hyperparameters.
  const double num_trains = ~a;
  const int nn_width = ~a;
  const int nn_depth = ~a;

  // Arrays for storing solution.
  double **u = (double**) malloc((nx + 2) * sizeof(double*));
  double **un = (double**) malloc((nx + 2) * sizeof(double*));
  for (int i = 0; i <= nx + 1; i++) {
    u[i] = (double*) malloc((ny + 2) * sizeof(double));
    un[i] = (double*) malloc((ny + 2) * sizeof(double));
  }

  // Arrays for storing training data.
  float **input_data = (float**) malloc(nx * ny * num_trains * sizeof(float*));
  float **output_data = (float**) malloc(nx * ny * num_trains * sizeof(float*));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    for (int j = 0;j <= ny + 1; j++) {
      double x = x0 + (i - 0.5) * dx;
      double y = y0 + (j - 0.5) * dy;
    
      u[i][j] = ~a; // init-func in C.
    }
  }

  // Initialize neural network architecture.
  kad_node_t *t_net;
  kann_t *ann;
  t_net = kann_layer_input(2);
  
  for (int i = 0; i < nn_depth; i++) {
    t_net = kann_layer_dense(t_net, nn_width);
    t_net = kad_tanh(t_net);
  }

  t_net = kann_layer_cost(t_net, 1, KANN_C_MSE);
  ann = kann_new(t_net, 0);

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha_x = 0.0;
    double alpha_y = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        double local_alpha_x = ~a; // max-speed-expr-x in C.
        double local_alpha_y = ~a; // max-speed-expr-y in C.
      
        if (local_alpha_x > alpha_x) {
          alpha_x = local_alpha_x;
        }
        if (local_alpha_y > alpha_y) {
          alpha_y = local_alpha_y;
        }
      }
    }

    // Avoid division by zero.
    if (alpha_x < 1e-14) {
      alpha_x = 1e-14;
    }
    if (alpha_y < 1e-14) {
      alpha_y = 1e-14;
    }

    // Compute stable time step from alpha.
    double dt = fmin(cfl * dx / alpha_x, cfl * dy / alpha_y);

    // If stepping beyond t_final, adjust dt accordingly.
    if (t + dt > t_final) {
      dt = t_final - t;
    }

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable in the x-direction.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        double um_x = u[i - 1][j];
        double ui_x = u[i][j];
        double up_x = u[i + 1][j];

        // Evaluate flux for each value of the conserved variable.
        double f_um_x = ~a; // f(u_{i - 1}).
        double f_ui_x = ~a; // f(u_i).
        double f_up_x = ~a; // f(u_{i + 1}).

        // Left interface flux: F_{i - 1/2} = 0.5 * (f(u_{i - 1}) + f(u_i)) - 0.5 * alpha_x * (u_i - u_{i - 1}).
        double fluxL_x = 0.5 * (f_um_x + f_ui_x) - 0.5 * alpha_x * (ui_x - um_x);

        // Right interface flux: F_{i + 1/2} = 0.5 * (f(u_{i + 1}) + f(u_i)) - 0.5 * alpha_x * (u_{i + 1} - u_i).
        double fluxR_x = 0.5 * (f_ui_x + f_up_x) - 0.5 * alpha_x * (up_x - ui_x);

        // Update the conserved variable.
        un[i][j] = ui_x - (dt / dx) * (fluxR_x - fluxL_x);
      }
    }

    // Copy un -> u (updated conserved variables to new conserved variables).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        u[i][j] = un[i][j];
      }
    }

    // Apply simple boundary conditions in the x-direction (transmissive).
    for (int j = 0; j <= ny + 1; j++) {
      u[0][j] = u[1][j];
      u[nx + 1][j] = u[nx][j];
    }

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable in the y-direction.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        double um_y = u[i][j - 1];
        double ui_y = u[i][j];
        double up_y = u[i][j + 1];
        
        // Evaluate flux for each value of the conserved variable.
        double f_um_y = ~a; // f(u_{j - 1}).
        double f_ui_y = ~a; // f(u_j).
        double f_up_y = ~a; // f(u_{j + 1}).

        // Left interface flux: F_{j - 1/2} = 0.5 * (f(u_{j - 1}) + f(u_j)) - 0.5 * alpha_y * (u_j - u_{j - 1}).
        double fluxL_y = 0.5 * (f_um_y + f_ui_y) - 0.5 * alpha_y * (ui_y - um_y);

        // Right interface flux: F_{j + 1/2} = 0.5 * (f(u_{j + 1}) + f(u_j)) - 0.5 * alpha_y * (u_{j + 1} - u_j).
        double fluxR_y = 0.5 * (f_ui_y + f_up_y) - 0.5 * alpha_y * (up_y - ui_y);

        // Update the conserved variable.
        un[i][j] = ui_y - (dt / dy) * (fluxR_y - fluxL_y);
      }
    }

    // Copy un -> u (updated conserved variables to new conserved variables).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        u[i][j] = un[i][j];
      }
    }

    // Apply simple boundary conditions in the y-direction (transmissive).
    for (int i = 0; i <= nx + 1; i++) {
      u[i][0] = u[i][1];
      u[i][ny + 1] = u[i][ny];
    }

    // Accumulate to training data.
    if (n < num_trains) {
      for (int i = 1; i <= nx; i++) {
        for (int j = 1; j<= ny; j++) {
          double x = x0 + (i - 0.5) * dx;
          double y = y0 + (j - 0.5) * dy;

          input_data[(n * nx * ny) + ((i - 1) * ny) + (j - 1)] = (float*) malloc(3 * sizeof(float));
          output_data[(n * nx * ny) + ((i - 1) * ny) + (j - 1)] = (float*) malloc(sizeof(float));
      
          input_data[(n * nx * ny) + ((i - 1) * ny) + (j - 1)][0] = t;
          input_data[(n * nx * ny) + ((i - 1) * ny) + (j - 1)][1] = x;
          input_data[(n * nx * ny) + ((i - 1) * ny) + (j - 1)][2] = y;
          output_data[(n * nx * ny) + ((i - 1) * ny) + (j - 1)][0] = u[i][j];
        }
      }
    }

    // Output solution to disk.
    const char *fmt = \"%s_output_%d.csv\";
    int sz = snprintf(0, 0, fmt, \"~a\", n);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", n);
    
    FILE *fptr = fopen(file_nm, \"w\");
    if (fptr != NULL) {
      for (int i = 1; i <= nx; i++) {
        for (int j = 1; j <= ny; j++) {
          double x = x0 + (i - 0.5) * dx;
          double y = y0 + (j - 0.5) * dy;
          fprintf(fptr, \"%f, %f, %f\\n\", x, y, u[i][j]);
        }
      }

      fclose(fptr);
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  // Train neural network.
  kann_train_fnn1(ann, 0.0001f, 64, 50, 10, 0.1f, n * nx * ny, input_data, output_data);

  // Output neural network to disk.
  const char *fmt = \"%s_neural_net.dat\";
  int sz = snprintf(0, 0, fmt, \"~a\");
  char file_nm[sz + 1];
  snprintf(file_nm, sizeof file_nm, fmt, \"~a\");
  
  kann_save(file_nm, ann);

  for (int i = 0; i <= nx + 1; i++) {
    free(u[i]);
    free(un[i]);
  }
  free(u);
  free(un);

  kann_delete(ann);
  
  for (int i = 0; i < nx * ny * num_trains; i++) {
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
           ;; Number of cells in each coordinate direction.
           nx
           ny
           ;; Left/right boundaries.
           x0
           x1
           ;; Up/down boundaries
           y0
           y1
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
           ;; Initial condition expression (e.g. (x < 1.0) ? 1.0 : 0.0)).
           init-func-code
           ;; Expressions for local wave-speed estimates.
           max-speed-local-x
           max-speed-local-y
           ;; Left, middle, right fluxes in x-direction f(u_{i - 1}), f(u_i), f(u_{i + 1}).
           flux-um-x
           flux-ui-x
           flux-up-x
           ;; Left, middle, right fluxes in y-direction f(u_{j - 1}), f(u_j), f(u_{j + 1}).
           flux-um-y
           flux-ui-y
           flux-up-y
           ;; PDE name for file output.
           name
           name
           ;; PDE name for neural network output.
           name
           name
           ))
  code)