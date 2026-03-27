#lang racket

(require "prover_core.rkt")
(require "prover_vector.rkt")
(require "code_generator_core.rkt")
(provide generate-lax-friedrichs-vector3-2d-conditional
         generate-roe-vector3-2d-conditional)

;; ---------------------------------------------------------------------------------------------------------------------------
;; Lax–Friedrichs (Finite-Difference) Solver for a 2D Coupled Vector System of 3 PDEs subject to certain algebraic constraints
;; ---------------------------------------------------------------------------------------------------------------------------
(define (generate-lax-friedrichs-vector3-2d-conditional pde-system conds epsilon
                                                        #:nx [nx 200]
                                                        #:ny [ny 200]
                                                        #:x0 [x0 0.0]
                                                        #:x1 [x1 2.0]
                                                        #:y0 [y0 0.0]
                                                        #:y1 [y1 2.0]
                                                        #:t-final [t-final 1.0]
                                                        #:cfl [cfl 0.95]
                                                        #:init-funcs [init-funcs (list
                                                                                  `(cond
                                                                                     [(< (+ (* (- x 1.0) (- x 1.0)) (* (- y 1.0) (- y 1.0))) 0.25) 5.0]
                                                                                     [else 1.0])
                                                                                  `0.0
                                                                                  `0.0)])
 "Generate C code that solves the 2D coupled vector system of 3 PDEs specified by `pde-system` using the Lax-Friedrichs finite-difference method,
  subject to the algebraic conditions `conds` with machine epsilon `epsilon`.
  - `nx`, `ny` : Number of spatial cells in each coordinate direction.
  - `x0`, `x1`, `y0`, `y1` : Domain boundaries in each coordinate direction.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-funcs`: Racket expressions for the initial conditions, e.g. piecewise constant."

  (define name (hash-ref pde-system 'name))
  (define cons-exprs (hash-ref pde-system 'cons-exprs))
  (define flux-exprs-x (hash-ref pde-system 'flux-exprs-x))
  (define flux-exprs-y (hash-ref pde-system 'flux-exprs-y))
  (define max-speed-exprs-x (hash-ref pde-system 'max-speed-exprs-x))
  (define max-speed-exprs-y (hash-ref pde-system 'max-speed-exprs-y))
  (define parameters (hash-ref pde-system 'parameters))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define flux-codes-x (map (lambda (flux-expr-x)
                              (convert-expr flux-expr-x)) flux-exprs-x))
  (define flux-codes-y (map (lambda (flux-expr-y)
                              (convert-expr flux-expr-y)) flux-exprs-y))
  (define max-speed-codes-x (map (lambda (max-speed-expr-x)
                                   (convert-expr max-speed-expr-x)) max-speed-exprs-x))
  (define max-speed-codes-y (map (lambda (max-speed-expr-y)
                                   (convert-expr max-speed-expr-y)) max-speed-exprs-y))
  (define init-func-codes (map (lambda (init-func-expr)
                                 (convert-expr init-func-expr)) init-funcs))

  (define flux-ums-x (map (lambda (flux-code-x)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-x (list-ref cons-codes 0) "um_x[0]")
                                                              (list-ref cons-codes 1) "um_x[1]") (list-ref cons-codes 2) "um_x[2]")) flux-codes-x))
  (define flux-uis-x (map (lambda (flux-code-x)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-x (list-ref cons-codes 0) "ui_x[0]")
                                                              (list-ref cons-codes 1) "ui_x[1]") (list-ref cons-codes 2) "ui_x[2]")) flux-codes-x))
  (define flux-ups-x (map (lambda (flux-code-x)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-x (list-ref cons-codes 0) "up_x[0]")
                                                              (list-ref cons-codes 1) "up_x[1]") (list-ref cons-codes 2) "up_x[2]")) flux-codes-x))

  (define flux-ums-y (map (lambda (flux-code-y)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-y (list-ref cons-codes 0) "um_y[0]")
                                                              (list-ref cons-codes 1) "um_y[1]") (list-ref cons-codes 2) "um_y[2]")) flux-codes-y))
  (define flux-uis-y (map (lambda (flux-code-y)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-y (list-ref cons-codes 0) "ui_y[0]")
                                                              (list-ref cons-codes 1) "ui_y[1]") (list-ref cons-codes 2) "ui_y[2]")) flux-codes-y))
  (define flux-ups-y (map (lambda (flux-code-y)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-y (list-ref cons-codes 0) "up_y[0]")
                                                              (list-ref cons-codes 1) "up_y[1]") (list-ref cons-codes 2) "up_y[2]")) flux-codes-y))

  (define max-speed-locals-x (map (lambda (max-speed-code-x)
                                    (flux-substitute (flux-substitute (flux-substitute max-speed-code-x (list-ref cons-codes 0) "u[i][(j * 3) + 0]")
                                                                      (list-ref cons-codes 1) "u[i][(j * 3) + 1]") (list-ref cons-codes 2) "u[i][(j * 3) + 2]")) max-speed-codes-x))
  (define max-speed-locals-y (map (lambda (max-speed-code-y)
                                    (flux-substitute (flux-substitute (flux-substitute max-speed-code-y (list-ref cons-codes 0) "u[i][(j * 3) + 0]")
                                                                      (list-ref cons-codes 1) "u[i][(j * 3) + 1]") (list-ref cons-codes 2) "u[i][(j * 3) + 2]")) max-speed-codes-y))

  (define epsilon-code (convert-expr epsilon))
  (define conditions-code (convert-expr (cons `and conds)))
  (define conditions-local (flux-substitute (flux-substitute
                                             (flux-substitute (flux-substitute
                                                               (flux-substitute (flux-substitute (flux-substitute conditions-code (list-ref cons-codes 0) "u[i][(j * 3) + 0]")
                                                                                                 (list-ref cons-codes 1) "u[i][(j * 3) + 1]") (list-ref cons-codes 2) "u[i][(j * 3) + 2]")
                                                               ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code))
                                             "> 0.0" (string-append "> -" epsilon-code)) "> 0" (string-append "> -" epsilon-code)))
  
  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR COUPLED VECTOR PDE SYSTEM: ~a
// Lax–Friedrichs first-order finite-difference solver for a coupled vector system of 3 PDEs in 2D subject to algebraic constraints.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

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

  // Arrays for storing solution.
  double **u = (double**) malloc((nx + 2) * sizeof(double*));
  double **un = (double**) malloc((nx + 2) * sizeof(double*));
  for (int i = 0; i <= nx + 1; i++) {
    u[i] = (double*) malloc((ny + 2) * 3 * sizeof(double));
    un[i] = (double*) malloc((ny + 2) * 3 * sizeof(double));
  }

  // Arrays for storing other intermediate values.
  double *local_alpha_x = (double*) malloc(3 * sizeof(double));
  double *local_alpha_y = (double*) malloc(3 * sizeof(double));

  double *um_x = (double*) malloc(3 * sizeof(double));
  double *ui_x = (double*) malloc(3 * sizeof(double));
  double *up_x = (double*) malloc(3 * sizeof(double));

  double *f_um_x = (double*) malloc(3 * sizeof(double));
  double *f_ui_x = (double*) malloc(3 * sizeof(double));
  double *f_up_x = (double*) malloc(3 * sizeof(double));

  double *fluxL_x = (double*) malloc(3 * sizeof(double));
  double *fluxR_x = (double*) malloc(3 * sizeof(double));

  double *um_y = (double*) malloc(3 * sizeof(double));
  double *ui_y = (double*) malloc(3 * sizeof(double));
  double *up_y = (double*) malloc(3 * sizeof(double));

  double *f_um_y = (double*) malloc(3 * sizeof(double));
  double *f_ui_y = (double*) malloc(3 * sizeof(double));
  double *f_up_y = (double*) malloc(3 * sizeof(double));

  double *fluxL_y = (double*) malloc(3 * sizeof(double));
  double *fluxR_y = (double*) malloc(3 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    for (int j = 0; j <= ny + 1; j++) {
      double x = x0 + (i - 0.5) * dx;
      double y = y0 + (j - 0.5) * dy;
    
      u[i][(j * 3) + 0] = ~a; // init-funcs[0] in C.
      u[i][(j * 3) + 1] = ~a; // init-funcs[1] in C.
      u[i][(j * 3) + 2] = ~a; // init-funcs[2] in C.

      un[i][(j * 3) + 0] = ~a; // init-funcs[0] in C.
      un[i][(j * 3) + 1] = ~a; // init-funcs[1] in C.
      un[i][(j * 3) + 2] = ~a; // init-funcs[2] in C.
    }
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha_x = 0.0;
    double alpha_y = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        local_alpha_x[0] = ~a; // max-speed-exprs-x[0] in C.
        local_alpha_x[1] = ~a; // max-speed-exprs-x[1] in C.
        local_alpha_x[2] = ~a; // max-speed-exprs-x[2] in C.

        local_alpha_y[0] = ~a; // max-speed-exprs-y[0] in C.
        local_alpha_y[1] = ~a; // max-speed-exprs-y[1] in C.
        local_alpha_y[2] = ~a; // max-speed-exprs-y[2] in C.
      
        for (int k = 0; k < 3; k++) {
          if (local_alpha_x[k] > alpha_x) {
            alpha_x = local_alpha_x[k];
          }
          if (local_alpha_y[k] > alpha_y) {
            alpha_y = local_alpha_y[k];
          }
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

    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        if (!(~a)) {
          printf(\"Time-step failed!\\n\");
          return 0;
        }
      }
    }

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable vector in the y-direction by half a time-step.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        for (int k = 0; k < 3; k++) {
          um_y[k] = u[i][((j - 1) * 3) + k];
          ui_y[k] = u[i][(j * 3) + k];
          up_y[k] = u[i][((j + 1) * 3) + k];
        }

        // Evaluate flux vector for each value of the conserved variable vector.
        f_um_y[0] = ~a;
        f_um_y[1] = ~a;
        f_um_y[2] = ~a; // F(U_{j - 1}).
      
        f_ui_y[0] = ~a;
        f_ui_y[1] = ~a;
        f_ui_y[2] = ~a; // F(U_j).
      
        f_up_y[0] = ~a;
        f_up_y[1] = ~a;
        f_up_y[2] = ~a; // F(U_{j + 1}).

        // Left interface flux: F_{j - 1/2} = 0.5 * (F(U_{j - 1}) + F(U_j)) - 0.5 * alpha_y * (U_j - U_{j - 1}).
        for (int k = 0; k < 3; k++) {
          fluxL_y[k] = 0.5 * (f_um_y[k] + f_ui_y[k]) - 0.5 * alpha_y * (ui_y[k] - um_y[k]);
        }

        // Right interface flux: F_{j + 1/2} = 0.5 * (F(U_{j + 1}) + F(U_j)) - 0.5 * alpha_y * (U_{j + 1} - U_j).
        for (int k = 0; k < 3; k++) {
          fluxR_y[k] = 0.5 * (f_ui_y[k] + f_up_y[k]) - 0.5 * alpha_y * (up_y[k] - ui_y[k]);
        }

        // Update the conserved variable vector.
        for (int k = 0; k < 3; k++) {
          un[i][(j * 3) + k] = ui_y[k] - (dt / (2.0 * dy)) * (fluxR_y[k] - fluxL_y[k]);
        }
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        for (int k = 0; k < 3; k++) {
          u[i][(j * 3) + k] = un[i][(j * 3) + k];
        }
      }
    }

    // Apply simple boundary conditions in the x-direction (transmissive).
    for (int j = 0; j <= ny + 1; j++) {
      for (int k = 0; k < 3; k++) {
        u[0][(j * 3) + k] = u[1][(j * 3) + k];
        u[nx + 1][(j * 3) + k] = u[nx][(j * 3) + k];

        un[0][(j * 3) + k] = un[1][(j * 3) + k];
        un[nx + 1][(j * 3) + k] = un[nx][(j * 3) + k];
      }
    }

    // Apply simple boundary conditions in the y-direction (transmissive).
    for (int i = 0; i <= nx + 1; i++) {
      for (int k = 0; k < 3; k++) {
        u[i][(0 * 3) + k] = u[i][(1 * 3) + k];
        u[i][((ny + 1) * 3) + k] = u[i][(ny * 3) + k];

        un[i][(0 * 3) + k] = un[i][(1 * 3) + k];
        un[i][((ny + 1) * 3) + k] = un[i][(ny * 3) + k];
      }
    }

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable vector in the x-direction.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        for (int k = 0; k < 3; k++) {
          um_x[k] = u[i - 1][(j * 3) + k];
          ui_x[k] = u[i][(j * 3) + k];
          up_x[k] = u[i + 1][(j * 3) + k];
        }

        // Evaluate flux vector for each value of the conserved variable vector.
        f_um_x[0] = ~a;
        f_um_x[1] = ~a;
        f_um_x[2] = ~a; // F(U_{i - 1}).
      
        f_ui_x[0] = ~a;
        f_ui_x[1] = ~a;
        f_ui_x[2] = ~a; // F(U_i).
      
        f_up_x[0] = ~a;
        f_up_x[1] = ~a;
        f_up_x[2] = ~a; // F(U_{i + 1}).

        // Left interface flux: F_{i - 1/2} = 0.5 * (F(U_{i - 1}) + F(U_i)) - 0.5 * alpha_x * (U_i - U_{i - 1}).
        for (int k = 0; k < 3; k++) {
          fluxL_x[k] = 0.5 * (f_um_x[k] + f_ui_x[k]) - 0.5 * alpha_x * (ui_x[k] - um_x[k]);
        }

        // Right interface flux: F_{i + 1/2} = 0.5 * (F(U_{i + 1}) + F(U_i)) - 0.5 * alpha_x * (U_{i + 1} - U_i).
        for (int k = 0; k < 3; k++) {
          fluxR_x[k] = 0.5 * (f_ui_x[k] + f_up_x[k]) - 0.5 * alpha_x * (up_x[k] - ui_x[k]);
        }

        // Update the conserved variable vector.
        for (int k = 0; k < 3; k++) {
          un[i][(j * 3) + k] = ui_x[k] - (dt / dx) * (fluxR_x[k] - fluxL_x[k]);
        }
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        for (int k = 0; k < 3; k++) {
          u[i][(j * 3) + k] = un[i][(j * 3) + k];
        }
      }
    }

    // Apply simple boundary conditions in the x-direction (transmissive).
    for (int j = 0; j <= ny + 1; j++) {
      for (int k = 0; k < 3; k++) {
        u[0][(j * 3) + k] = u[1][(j * 3) + k];
        u[nx + 1][(j * 3) + k] = u[nx][(j * 3) + k];

        un[0][(j * 3) + k] = un[1][(j * 3) + k];
        un[nx + 1][(j * 3) + k] = un[nx][(j * 3) + k];
      }
    }

    // Apply simple boundary conditions in the y-direction (transmissive).
    for (int i = 0; i <= nx + 1; i++) {
      for (int k = 0; k < 3; k++) {
        u[i][(0 * 3) + k] = u[i][(1 * 3) + k];
        u[i][((ny + 1) * 3) + k] = u[i][(ny * 3) + k];

        un[i][(0 * 3) + k] = un[i][(1 * 3) + k];
        un[i][((ny + 1) * 3) + k] = un[i][(ny * 3) + k];
      }
    }

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable vector in the y-direction by half a time-step.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        for (int k = 0; k < 3; k++) {
          um_y[k] = u[i][((j - 1) * 3) + k];
          ui_y[k] = u[i][(j * 3) + k];
          up_y[k] = u[i][((j + 1) * 3) + k];
        }

        // Evaluate flux vector for each value of the conserved variable vector.
        f_um_y[0] = ~a;
        f_um_y[1] = ~a;
        f_um_y[2] = ~a; // F(U_{j - 1}).
      
        f_ui_y[0] = ~a;
        f_ui_y[1] = ~a;
        f_ui_y[2] = ~a; // F(U_j).
      
        f_up_y[0] = ~a;
        f_up_y[1] = ~a;
        f_up_y[2] = ~a; // F(U_{j + 1}).

        // Left interface flux: F_{j - 1/2} = 0.5 * (F(U_{j - 1}) + F(U_j)) - 0.5 * alpha_y * (U_j - U_{j - 1}).
        for (int k = 0; k < 3; k++) {
          fluxL_y[k] = 0.5 * (f_um_y[k] + f_ui_y[k]) - 0.5 * alpha_y * (ui_y[k] - um_y[k]);
        }

        // Right interface flux: F_{j + 1/2} = 0.5 * (F(U_{j + 1}) + F(U_j)) - 0.5 * alpha_y * (U_{j + 1} - U_j).
        for (int k = 0; k < 3; k++) {
          fluxR_y[k] = 0.5 * (f_ui_y[k] + f_up_y[k]) - 0.5 * alpha_y * (up_y[k] - ui_y[k]);
        }

        // Update the conserved variable vector.
        for (int k = 0; k < 3; k++) {
          un[i][(j * 3) + k] = ui_y[k] - (dt / (2.0 * dy)) * (fluxR_y[k] - fluxL_y[k]);
        }
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        for (int k = 0; k < 3; k++) {
          u[i][(j * 3) + k] = un[i][(j * 3) + k];
        }
      }
    }

    // Apply simple boundary conditions in the x-direction (transmissive).
    for (int j = 0; j <= ny + 1; j++) {
      for (int k = 0; k < 3; k++) {
        u[0][(j * 3) + k] = u[1][(j * 3) + k];
        u[nx + 1][(j * 3) + k] = u[nx][(j * 3) + k];

        un[0][(j * 3) + k] = un[1][(j * 3) + k];
        un[nx + 1][(j * 3) + k] = un[nx][(j * 3) + k];
      }
    }

    // Apply simple boundary conditions in the y-direction (transmissive).
    for (int i = 0; i <= nx + 1; i++) {
      for (int k = 0; k < 3; k++) {
        u[i][(0 * 3) + k] = u[i][(1 * 3) + k];
        u[i][((ny + 1) * 3) + k] = u[i][(ny * 3) + k];

        un[i][(0 * 3) + k] = un[i][(1 * 3) + k];
        un[i][((ny + 1) * 3) + k] = un[i][(ny * 3) + k];
      }
    }

    // Output solution to disk.
    for (int k = 0; k < 3; k++) {
      const char *fmt = \"%s_output_%d_%d.csv\";
      int sz = snprintf(0, 0, fmt, \"~a\", k, n);
      char file_nm[sz + 1];
      snprintf(file_nm, sizeof file_nm, fmt, \"~a\", k, n);
    
      FILE *fptr = fopen(file_nm, \"w\");
      if (fptr != NULL) {
        for (int i = 1; i <= nx; i++) {
          for (int j = 1; j <= ny; j++) {
            double x = x0 + (i - 0.5) * dx;
            double y = y0 + (j - 0.5) * dy;
            fprintf(fptr, \"%f, %f, %f\\n\", x, y, u[i][(j * 3) + k]);
          }
        }

        fclose(fptr);
      }
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  for (int i = 0; i <= nx + 1; i++) {
    free(u[i]);
    free(un[i]);
  }
  free(u);
  free(un);

  free(local_alpha_x);
  free(local_alpha_y);
  
  free(um_x);
  free(ui_x);
  free(up_x);

  free(f_um_x);
  free(f_ui_x);
  free(f_up_x);

  free(fluxL_x);
  free(fluxR_x);

  free(um_y);
  free(ui_y);
  free(up_y);

  free(f_um_y);
  free(f_ui_y);
  free(f_up_y);

  free(fluxL_y);
  free(fluxR_y);
  
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
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           (list-ref init-func-codes 2)
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           (list-ref init-func-codes 2)
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals-x 0)
           (list-ref max-speed-locals-x 1)
           (list-ref max-speed-locals-x 2)
           (list-ref max-speed-locals-y 0)
           (list-ref max-speed-locals-y 1)
           (list-ref max-speed-locals-y 2)
           ;; Expression for algebraic constraints.
           conditions-local
           ;; Left, middle, right flux vectors in y-direction F(u_{j - 1}), F(u_j), F(u_{j + 1}).
           (list-ref flux-ums-y 0)
           (list-ref flux-ums-y 1)
           (list-ref flux-ums-y 2)
           (list-ref flux-uis-y 0)
           (list-ref flux-uis-y 1)
           (list-ref flux-uis-y 2)
           (list-ref flux-ups-y 0)
           (list-ref flux-ups-y 1)
           (list-ref flux-ups-y 2)
           ;; Left, middle, right flux vectors in x-direction F(u_{i - 1}), F(u_i), F(u_{i + 1}).
           (list-ref flux-ums-x 0)
           (list-ref flux-ums-x 1)
           (list-ref flux-ums-x 2)
           (list-ref flux-uis-x 0)
           (list-ref flux-uis-x 1)
           (list-ref flux-uis-x 2)
           (list-ref flux-ups-x 0)
           (list-ref flux-ups-x 1)
           (list-ref flux-ups-x 2)
           ;; Left, middle, right flux vectors in y-direction F(u_{j - 1}), F(u_j), F(u_{j + 1}).
           (list-ref flux-ums-y 0)
           (list-ref flux-ums-y 1)
           (list-ref flux-ums-y 2)
           (list-ref flux-uis-y 0)
           (list-ref flux-uis-y 1)
           (list-ref flux-uis-y 2)
           (list-ref flux-ups-y 0)
           (list-ref flux-ups-y 1)
           (list-ref flux-ups-y 2)
           ;; PDE name for file output.
           name
           name
           ))
  code)

;; ------------------------------------------------------------------------------------------------------------
;; Roe (Finite-Volume) Solver for a 2D Coupled Vector System of 3 PDEs subject to certain algebraic constraints
;; ------------------------------------------------------------------------------------------------------------
(define (generate-roe-vector3-2d-conditional pde-system conds epsilon
                                             #:nx [nx 200]
                                             #:ny [ny 200]
                                             #:x0 [x0 0.0]
                                             #:x1 [x1 2.0]
                                             #:y0 [y0 0.0]
                                             #:y1 [y1 2.0]
                                             #:t-final [t-final 1.0]
                                             #:cfl [cfl 0.95]
                                             #:init-funcs [init-funcs (list
                                                                       `(cond
                                                                          [(< (+ (* (- x 1.0) (- x 1.0)) (* (- y 1.0) (- y 1.0))) 0.25) 5.0]
                                                                          [else 1.0])
                                                                       `0.0
                                                                       `0.0)])
 "Generate C code that solves the 2D coupled vector system of 3 PDEs specified by `pde-system` using the Roe finite-volume method,
  subject to the algebraic conditions `conds` with machine epsilon `epsilon`.
  - `nx`, `ny` : Number of spatial cells in each coordinate direction.
  - `x0`, `x1`, `y0`, `y1` : Domain boundaries in each coordinate direction.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-funcs`: Racket expressions for the initial conditions, e.g. piecewise constant."

  (define name (hash-ref pde-system 'name))
  (define cons-exprs (hash-ref pde-system 'cons-exprs))
  (define flux-exprs-x (hash-ref pde-system 'flux-exprs-x))
  (define flux-exprs-y (hash-ref pde-system 'flux-exprs-y))
  (define max-speed-exprs-x (hash-ref pde-system 'max-speed-exprs-x))
  (define max-speed-exprs-y (hash-ref pde-system 'max-speed-exprs-y))
  (define parameters (hash-ref pde-system 'parameters))

  (define flux-jacobian-eigvals-x (symbolic-eigvals3 (symbolic-jacobian flux-exprs-x cons-exprs)))
  (define flux-jacobian-eigvals-y (symbolic-eigvals3 (symbolic-jacobian flux-exprs-y cons-exprs)))
  (define flux-jacobian-eigvals-simp-x (list (symbolic-simp (list-ref flux-jacobian-eigvals-x 0))
                                             (symbolic-simp (list-ref flux-jacobian-eigvals-x 1))
                                             (symbolic-simp (list-ref flux-jacobian-eigvals-x 2))))
  (define flux-jacobian-eigvals-simp-y (list (symbolic-simp (list-ref flux-jacobian-eigvals-y 0))
                                             (symbolic-simp (list-ref flux-jacobian-eigvals-y 1))
                                             (symbolic-simp (list-ref flux-jacobian-eigvals-y 2))))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define flux-codes-x (map (lambda (flux-expr-x)
                              (convert-expr flux-expr-x)) flux-exprs-x))
  (define flux-codes-y (map (lambda (flux-expr-y)
                              (convert-expr flux-expr-y)) flux-exprs-y))
  (define flux-deriv-codes-x (map (lambda (flux-deriv-expr-x)
                                    (convert-expr flux-deriv-expr-x)) flux-jacobian-eigvals-simp-x))
  (define flux-deriv-codes-y (map (lambda (flux-deriv-expr-y)
                                    (convert-expr flux-deriv-expr-y)) flux-jacobian-eigvals-simp-y))
  (define max-speed-codes-x (map (lambda (max-speed-expr-x)
                                   (convert-expr max-speed-expr-x)) max-speed-exprs-x))
  (define max-speed-codes-y (map (lambda (max-speed-expr-y)
                                   (convert-expr max-speed-expr-y)) max-speed-exprs-y))
  (define init-func-codes (map (lambda (init-func-expr)
                                 (convert-expr init-func-expr)) init-funcs))

  (define flux-ums-x (map (lambda (flux-code-x)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-x (list-ref cons-codes 0) "um_x[0]")
                                                              (list-ref cons-codes 1) "um_x[1]") (list-ref cons-codes 2) "um_x[2]")) flux-codes-x))
  (define flux-uis-x (map (lambda (flux-code-x)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-x (list-ref cons-codes 0) "ui_x[0]")
                                                              (list-ref cons-codes 1) "ui_x[1]") (list-ref cons-codes 2) "ui_x[2]")) flux-codes-x))
  (define flux-ups-x (map (lambda (flux-code-x)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-x (list-ref cons-codes 0) "up_x[0]")
                                                              (list-ref cons-codes 1) "up_x[1]") (list-ref cons-codes 2) "up_x[2]")) flux-codes-x))

  (define flux-deriv-ums-x (map (lambda (flux-deriv-code-x)
                                  (flux-substitute (flux-substitute (flux-substitute flux-deriv-code-x (list-ref cons-codes 0) "um_x[0]")
                                                                    (list-ref cons-codes 1) "um_x[1]") (list-ref cons-codes 2) "um_x[2]")) flux-deriv-codes-x))
  (define flux-deriv-uis-x (map (lambda (flux-deriv-code-x)
                                  (flux-substitute (flux-substitute (flux-substitute flux-deriv-code-x (list-ref cons-codes 0) "ui_x[0]")
                                                                    (list-ref cons-codes 1) "ui_x[1]") (list-ref cons-codes 2) "ui_x[2]")) flux-deriv-codes-x))
  (define flux-deriv-ups-x (map (lambda (flux-deriv-code-x)
                                  (flux-substitute (flux-substitute (flux-substitute flux-deriv-code-x (list-ref cons-codes 0) "up_x[0]")
                                                                    (list-ref cons-codes 1) "up_x[1]") (list-ref cons-codes 2) "up_x[2]")) flux-deriv-codes-x))

  (define flux-ums-y (map (lambda (flux-code-y)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-y (list-ref cons-codes 0) "um_y[0]")
                                                              (list-ref cons-codes 1) "um_y[1]") (list-ref cons-codes 2) "um_y[2]")) flux-codes-y))
  (define flux-uis-y (map (lambda (flux-code-y)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-y (list-ref cons-codes 0) "ui_y[0]")
                                                              (list-ref cons-codes 1) "ui_y[1]") (list-ref cons-codes 2) "ui_y[2]")) flux-codes-y))
  (define flux-ups-y (map (lambda (flux-code-y)
                            (flux-substitute (flux-substitute (flux-substitute flux-code-y (list-ref cons-codes 0) "up_y[0]")
                                                              (list-ref cons-codes 1) "up_y[1]") (list-ref cons-codes 2) "up_y[2]")) flux-codes-y))

  (define flux-deriv-ums-y (map (lambda (flux-deriv-code-y)
                                  (flux-substitute (flux-substitute (flux-substitute flux-deriv-code-y (list-ref cons-codes 0) "um_y[0]")
                                                                    (list-ref cons-codes 1) "um_y[1]") (list-ref cons-codes 2) "um_y[2]")) flux-deriv-codes-y))
  (define flux-deriv-uis-y (map (lambda (flux-deriv-code-y)
                                  (flux-substitute (flux-substitute (flux-substitute flux-deriv-code-y (list-ref cons-codes 0) "ui_y[0]")
                                                                    (list-ref cons-codes 1) "ui_y[1]") (list-ref cons-codes 2) "ui_y[2]")) flux-deriv-codes-y))
  (define flux-deriv-ups-y (map (lambda (flux-deriv-code-y)
                                  (flux-substitute (flux-substitute (flux-substitute flux-deriv-code-y (list-ref cons-codes 0) "up_y[0]")
                                                                    (list-ref cons-codes 1) "up_y[1]") (list-ref cons-codes 2) "up_y[2]")) flux-deriv-codes-y))
  
  (define max-speed-locals-x (map (lambda (max-speed-code-x)
                                    (flux-substitute (flux-substitute (flux-substitute max-speed-code-x (list-ref cons-codes 0) "u[i][(j * 3) + 0]")
                                                                      (list-ref cons-codes 1) "u[i][(j * 3) + 1]") (list-ref cons-codes 2) "u[i][(j * 3) + 2]")) max-speed-codes-x))
  (define max-speed-locals-y (map (lambda (max-speed-code-y)
                                    (flux-substitute (flux-substitute (flux-substitute max-speed-code-y (list-ref cons-codes 0) "u[i][(j * 3) + 0]")
                                                                      (list-ref cons-codes 1) "u[i][(j * 3) + 1]") (list-ref cons-codes 2) "u[i][(j * 3) + 2]")) max-speed-codes-y))

  (define cons-codes-left (list (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 0)) "L")))
                                (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 1)) "L")))
                                (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 2)) "L")))))
  (define cons-codes-right (list (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 0)) "R")))
                                 (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 1)) "R")))
                                 (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 2)) "R")))))

  (define epsilon-code (convert-expr epsilon))
  (define conditions-code (convert-expr (cons `and conds)))
  
  (define conditions-local-top (flux-substitute (flux-substitute
                                                 (flux-substitute (flux-substitute
                                                                   (flux-substitute (flux-substitute
                                                                                     (flux-substitute (flux-substitute
                                                                                                       (flux-substitute (flux-substitute
                                                                                                                         conditions-code (list-ref cons-codes-left 0) "u[i][((j - 1) * 3) + 0]")
                                                                                                                        (list-ref cons-codes-left 1) "u[i][((j - 1) * 3) + 1]")
                                                                                                       (list-ref cons-codes-left 2) "u[i][((j - 1) * 3) + 2]")
                                                                                                      (list-ref cons-codes-right 0) "u[i][(j * 3) + 0]")
                                                                                     (list-ref cons-codes-right 1) "u[i][(j * 3) + 1]")
                                                                                    (list-ref cons-codes-right 2) "u[i][(j * 3) + 2]")
                                                                   ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code))
                                                 "== 0.0" (string-append "< " epsilon-code)) "== 0" (string-append "< " epsilon-code)))
  (define conditions-local-bottom (flux-substitute (flux-substitute
                                                    (flux-substitute (flux-substitute
                                                                      (flux-substitute (flux-substitute
                                                                                        (flux-substitute (flux-substitute
                                                                                                          (flux-substitute (flux-substitute
                                                                                                                            conditions-code (list-ref cons-codes-left 0) "u[i][(j * 3) + 0]")
                                                                                                                           (list-ref cons-codes-left 1) "u[i][(j * 3) + 1]")
                                                                                                          (list-ref cons-codes-left 2) "u[i][(j * 3) + 2]")
                                                                                                         (list-ref cons-codes-right 0) "u[i][((j + 1) * 3) + 0]")
                                                                                        (list-ref cons-codes-right 1) "u[i][((j + 1) * 3) + 1]")
                                                                                       (list-ref cons-codes-right 2) "u[i][((j + 1) * 3) + 2]")
                                                                      ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code))
                                                    "== 0.0" (string-append "< " epsilon-code)) "== 0" (string-append "< " epsilon-code)))

  (define conditions-local-left (flux-substitute (flux-substitute
                                                  (flux-substitute (flux-substitute
                                                                    (flux-substitute (flux-substitute
                                                                                      (flux-substitute (flux-substitute
                                                                                                        (flux-substitute (flux-substitute
                                                                                                                          conditions-code (list-ref cons-codes-left 0) "u[i - 1][(j * 3) + 0]")
                                                                                                                         (list-ref cons-codes-left 1) "u[i - 1][(j * 3) + 1]")
                                                                                                        (list-ref cons-codes-left 2) "u[i - 1][(j * 3) + 2]")
                                                                                                       (list-ref cons-codes-right 0) "u[i][(j * 3) + 0]")
                                                                                      (list-ref cons-codes-right 1) "u[i][(j * 3) + 1]")
                                                                                     (list-ref cons-codes-right 2) "u[i][(j * 3) + 2]")
                                                                    ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code))
                                                  "== 0.0" (string-append "< " epsilon-code)) "== 0" (string-append "< " epsilon-code)))
  (define conditions-local-right (flux-substitute (flux-substitute
                                                   (flux-substitute (flux-substitute
                                                                     (flux-substitute (flux-substitute
                                                                                       (flux-substitute (flux-substitute
                                                                                                         (flux-substitute (flux-substitute
                                                                                                                           conditions-code (list-ref cons-codes-left 0) "u[i][(j * 3) + 0]")
                                                                                                                          (list-ref cons-codes-left 1) "u[i][(j * 3) + 1]")
                                                                                                         (list-ref cons-codes-left 2) "u[i][(j * 3) + 2]")
                                                                                                        (list-ref cons-codes-right 0) "u[i + 1][(j * 3) + 0]")
                                                                                       (list-ref cons-codes-right 1) "u[i + 1][(j * 3) + 1]")
                                                                                      (list-ref cons-codes-right 2) "u[i + 1][(j * 3) + 2]")
                                                                     ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code))
                                                   "== 0.0" (string-append "< " epsilon-code)) "== 0" (string-append "< " epsilon-code)))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))
  
  (define code
    (format "
// AUTO-GENERATED CODE FOR COUPLED VECTOR PDE SYSTEM: ~a
// Roe higher-order finite-volume solver for a coupled vector system of 3 PDEs in 2D subject to certain algebraic constraints.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

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

  // Arrays for storing solution.
  double **u = (double**) malloc((nx + 2) * sizeof(double*));
  double **un = (double**) malloc((nx + 2) * sizeof(double*));
  for (int i = 0; i <= nx + 1; i++) {
    u[i] = (double*) malloc((ny + 2) * 3 * sizeof(double));
    un[i] = (double*) malloc((ny + 2) * 3 * sizeof(double));
  }

  // Arrays for storing other intermediate values.
  double *local_alpha_x = (double*) malloc(3 * sizeof(double));
  double *local_alpha_y = (double*) malloc(3 * sizeof(double));

  double *um_x = (double*) malloc(3 * sizeof(double));
  double *ui_x = (double*) malloc(3 * sizeof(double));
  double *up_x = (double*) malloc(3 * sizeof(double));

  double *f_um_x = (double*) malloc(3 * sizeof(double));
  double *f_ui_x = (double*) malloc(3 * sizeof(double));
  double *f_up_x = (double*) malloc(3 * sizeof(double));

  double *f_deriv_um_x = (double*) malloc(3 * sizeof(double));
  double *f_deriv_ui_x = (double*) malloc(3 * sizeof(double));
  double *f_deriv_up_x = (double*) malloc(3 * sizeof(double));

  double *aL_roe_x = (double*) malloc(3 * sizeof(double));
  double *aR_roe_x = (double*) malloc(3 * sizeof(double));

  double *fluxL_x = (double*) malloc(3 * sizeof(double));
  double *fluxR_x = (double*) malloc(3 * sizeof(double));

  double *um_y = (double*) malloc(3 * sizeof(double));
  double *ui_y = (double*) malloc(3 * sizeof(double));
  double *up_y = (double*) malloc(3 * sizeof(double));

  double *f_um_y = (double*) malloc(3 * sizeof(double));
  double *f_ui_y = (double*) malloc(3 * sizeof(double));
  double *f_up_y = (double*) malloc(3 * sizeof(double));

  double *f_deriv_um_y = (double*) malloc(3 * sizeof(double));
  double *f_deriv_ui_y = (double*) malloc(3 * sizeof(double));
  double *f_deriv_up_y = (double*) malloc(3 * sizeof(double));

  double *aL_roe_y = (double*) malloc(3 * sizeof(double));
  double *aR_roe_y = (double*) malloc(3 * sizeof(double));

  double *fluxL_y = (double*) malloc(3 * sizeof(double));
  double *fluxR_y = (double*) malloc(3 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    for (int j = 0; j <= ny + 1; j++) {
      double x = x0 + (i - 0.5) * dx;
      double y = y0 + (j - 0.5) * dy;
    
      u[i][(j * 3) + 0] = ~a; // init-funcs[0] in C.
      u[i][(j * 3) + 1] = ~a; // init-funcs[1] in C.
      u[i][(j * 3) + 2] = ~a; // init-funcs[2] in C.

      un[i][(j * 3) + 0] = ~a; // init-funcs[0] in C.
      un[i][(j * 3) + 1] = ~a; // init-funcs[1] in C.
      un[i][(j * 3) + 2] = ~a; // init-funcs[2] in C.
    }
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha_x = 0.0;
    double alpha_y = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        local_alpha_x[0] = ~a; // max-speed-exprs-x[0] in C.
        local_alpha_x[1] = ~a; // max-speed-exprs-x[1] in C.
        local_alpha_x[2] = ~a; // max-speed-exprs-x[2] in C.

        local_alpha_y[0] = ~a; // max-speed-exprs-y[0] in C.
        local_alpha_y[1] = ~a; // max-speed-exprs-y[1] in C.
        local_alpha_y[2] = ~a; // max-speed-exprs-y[2] in C.
      
        for (int k = 0; k < 3; k++) {
          if (local_alpha_x[k] > alpha_x) {
            alpha_x = local_alpha_x[k];
          }
          if (local_alpha_y[k] > alpha_y) {
            alpha_y = local_alpha_y[k];
          }
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

    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        if (!(~a)) {
          printf(\"Time-step failed!\\n\");
          return 0;
        }

        if (!(~a)) {
          printf(\"Time-step failed!\\n\");
          return 0;
        }

        if (!(~a)) {
          printf(\"Time-step failed!\\n\");
          return 0;
        }

        if (!(~a)) {
          printf(\"Time-step failed!\\n\");
          return 0;
        }
      }
    }

    // Compute fluxes with Roe approximation and update the conserved variable vector in the y-direction by half a time-step.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        for (int k = 0; k < 3; k++) {
          um_y[k] = u[i][((j - 1) * 3) + k];
          ui_y[k] = u[i][(j * 3) + k];
          up_y[k] = u[i][((j + 1) * 3) + k];
        }
        
        // Evaluate flux vector for each value of the conserved variable vector.
        f_um_y[0] = ~a;
        f_um_y[1] = ~a;
        f_um_y[2] = ~a; // F(U_{j - 1}).
        
        f_ui_y[0] = ~a;
        f_ui_y[1] = ~a;
        f_ui_y[2] = ~a; // F(U_j).
      
        f_up_y[0] = ~a;
        f_up_y[1] = ~a;
        f_up_y[2] = ~a; // F(U_{j + 1}).

        // Evaluate eigenvalues of the flux Jacobian for each value of the conserved variable vector.
        f_deriv_um_y[0] = ~a;
        f_deriv_um_y[1] = ~a;
        f_deriv_um_y[2] = ~a; // Eigenvalues of F'(U_{j - 1}).
      
        f_deriv_ui_y[0] = ~a;
        f_deriv_ui_y[1] = ~a;
        f_deriv_ui_y[2] = ~a; // Eigenvalues of F'(U_j).
      
        f_deriv_up_y[0] = ~a;
        f_deriv_up_y[1] = ~a;
        f_deriv_up_y[2] = ~a; // Eigenvalues of F'(U_{j + 1}).
        
        // Left interface flux: F_{j - 1/2} = 0.5 * (F(U_{j - 1}) + F(U_j)) - 0.5 * |aL_roe_y| * (U_j - U_{j - 1}).
        for (int k = 0; k < 3; k++) {
          aL_roe_y[k] = 0.5 * (f_deriv_um_y[k] + f_deriv_ui_y[k]);
        }
        for (int k = 0; k < 3; k++) {
          fluxL_y[k] = 0.5 * (f_um_y[k] + f_ui_y[k]) - 0.5 * fabs(aL_roe_y[k]) * (ui_y[k] - um_y[k]);
        }

        // Right interface flux: F_{j + 1/2} = 0.5 * (F(U_{j + 1}) + F(U_j)) - 0.5 * |aR_roe_y| * (U_{j + 1} - U_j).
        for (int k = 0; k < 3; k++) {
          aR_roe_y[k] = 0.5 * (f_deriv_ui_y[k] + f_deriv_up_y[k]);
        }
        for (int k = 0; k < 3; k++) {
          fluxR_y[k] = 0.5 * (f_ui_y[k] + f_up_y[k]) - 0.5 * fabs(aR_roe_y[k]) * (up_y[k] - ui_y[k]);
        }

        // Update the conserved variable vector.
        for (int k = 0; k < 3; k++) {
          un[i][(j * 3) + k] = ui_y[k] - (dt / (2.0 * dy)) * (fluxR_y[k] - fluxL_y[k]);
        }
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        for (int k = 0; k < 3; k++) {
          u[i][(j * 3) + k] = un[i][(j * 3) + k];
        }
      }
    }

    // Apply simple boundary conditions in the x-direction (transmissive).
    for (int j = 0; j <= ny + 1; j++) {
      for (int k = 0; k < 3; k++) {
        u[0][(j * 3) + k] = u[1][(j * 3) + k];
        u[nx + 1][(j * 3) + k] = u[nx][(j * 3) + k];

        un[0][(j * 3) + k] = un[1][(j * 3) + k];
        un[nx + 1][(j * 3) + k] = un[nx][(j * 3) + k];
      }
    }

    // Apply simple boundary conditions in the y-direction (transmissive).
    for (int i = 0; i <= nx + 1; i++) {
      for (int k = 0; k < 3; k++) {
        u[i][(0 * 3) + k] = u[i][(1 * 3) + k];
        u[i][((ny + 1) * 3) + k] = u[i][(ny * 3) + k];

        un[i][(0 * 3) + k] = un[i][(1 * 3) + k];
        un[i][((ny + 1) * 3) + k] = un[i][(ny * 3) + k];
      }
    }

    // Compute fluxes with Roe approximation and update the conserved variable vector in the x-direction.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        for (int k = 0; k < 3; k++) {
          um_x[k] = u[i - 1][(j * 3) + k];
          ui_x[k] = u[i][(j * 3) + k];
          up_x[k] = u[i + 1][(j * 3) + k];
        }

        // Evaluate flux vector for each value of the conserved variable vector.
        f_um_x[0] = ~a;
        f_um_x[1] = ~a;
        f_um_x[2] = ~a; // F(U_{i - 1}).
      
        f_ui_x[0] = ~a;
        f_ui_x[1] = ~a;
        f_ui_x[2] = ~a; // F(U_i).
      
        f_up_x[0] = ~a;
        f_up_x[1] = ~a;
        f_up_x[2] = ~a; // F(U_{i + 1}).

        // Evaluate eigenvalues of the flux Jacobian for each value of the conserved variable vector.
        f_deriv_um_x[0] = ~a;
        f_deriv_um_x[1] = ~a;
        f_deriv_um_x[2] = ~a; // Eigenvalues of F'(U_{i - 1}).
      
        f_deriv_ui_x[0] = ~a;
        f_deriv_ui_x[1] = ~a;
        f_deriv_ui_x[2] = ~a; // Eigenvalues of F'(U_i).
      
        f_deriv_up_x[0] = ~a;
        f_deriv_up_x[1] = ~a;
        f_deriv_up_x[2] = ~a; // Eigenvalues of F'(U_{i + 1}).
        
        // Left interface flux: F_{i - 1/2} = 0.5 * (F(U_{i - 1}) + F(U_i)) - 0.5 * |aL_roe_x| * (U_i - U_{i - 1}).
        for (int k = 0; k < 3; k++) {
          aL_roe_x[k] = 0.5 * (f_deriv_um_x[k] + f_deriv_ui_x[k]);
        }
        for (int k = 0; k < 3; k++) {
          fluxL_x[k] = 0.5 * (f_um_x[k] + f_ui_x[k]) - 0.5 * fabs(aL_roe_x[k]) * (ui_x[k] - um_x[k]);
        }

        // Right interface flux: F_{i + 1/2} = 0.5 * (F(U_{i + 1}) + F(U_i)) - 0.5 * |aR_roe_x| * (U_{i + 1} - U_i).
        for (int k = 0; k < 3; k++) {
          aR_roe_x[k] = 0.5 * (f_deriv_ui_x[k] + f_deriv_up_x[k]);
        }
        for (int k = 0; k < 3; k++) {
          fluxR_x[k] = 0.5 * (f_ui_x[k] + f_up_x[k]) - 0.5 * fabs(aR_roe_x[k]) * (up_x[k] - ui_x[k]);
        }

        // Update the conserved variable vector.
        for (int k = 0; k < 3; k++) {
          un[i][(j * 3) + k] = ui_x[k] - (dt / dx) * (fluxR_x[k] - fluxL_x[k]);
        }
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        for (int k = 0; k < 3; k++) {
          u[i][(j * 3) + k] = un[i][(j * 3) + k];
        }
      }
    }

    // Apply simple boundary conditions in the x-direction (transmissive).
    for (int j = 0; j <= ny + 1; j++) {
      for (int k = 0; k < 3; k++) {
        u[0][(j * 3) + k] = u[1][(j * 3) + k];
        u[nx + 1][(j * 3) + k] = u[nx][(j * 3) + k];

        un[0][(j * 3) + k] = un[1][(j * 3) + k];
        un[nx + 1][(j * 3) + k] = un[nx][(j * 3) + k];
      }
    }

    // Apply simple boundary conditions in the y-direction (transmissive).
    for (int i = 0; i <= nx + 1; i++) {
      for (int k = 0; k < 3; k++) {
        u[i][(0 * 3) + k] = u[i][(1 * 3) + k];
        u[i][((ny + 1) * 3) + k] = u[i][(ny * 3) + k];

        un[i][(0 * 3) + k] = un[i][(1 * 3) + k];
        un[i][((ny + 1) * 3) + k] = un[i][(ny * 3) + k];
      }
    }

    // Compute fluxes with Roe approximation and update the conserved variable vector in the y-direction by half a time-step.
    for (int i = 1; i <= nx; i++) {
      for (int j = 1; j <= ny; j++) {
        for (int k = 0; k < 3; k++) {
          um_y[k] = u[i][((j - 1) * 3) + k];
          ui_y[k] = u[i][(j * 3) + k];
          up_y[k] = u[i][((j + 1) * 3) + k];
        }
        
        // Evaluate flux vector for each value of the conserved variable vector.
        f_um_y[0] = ~a;
        f_um_y[1] = ~a;
        f_um_y[2] = ~a; // F(U_{j - 1}).
        
        f_ui_y[0] = ~a;
        f_ui_y[1] = ~a;
        f_ui_y[2] = ~a; // F(U_j).
      
        f_up_y[0] = ~a;
        f_up_y[1] = ~a;
        f_up_y[2] = ~a; // F(U_{j + 1}).

        // Evaluate eigenvalues of the flux Jacobian for each value of the conserved variable vector.
        f_deriv_um_y[0] = ~a;
        f_deriv_um_y[1] = ~a;
        f_deriv_um_y[2] = ~a; // Eigenvalues of F'(U_{j - 1}).
      
        f_deriv_ui_y[0] = ~a;
        f_deriv_ui_y[1] = ~a;
        f_deriv_ui_y[2] = ~a; // Eigenvalues of F'(U_j).
      
        f_deriv_up_y[0] = ~a;
        f_deriv_up_y[1] = ~a;
        f_deriv_up_y[2] = ~a; // Eigenvalues of F'(U_{j + 1}).
        
        // Left interface flux: F_{j - 1/2} = 0.5 * (F(U_{j - 1}) + F(U_j)) - 0.5 * |aL_roe_y| * (U_j - U_{j - 1}).
        for (int k = 0; k < 3; k++) {
          aL_roe_y[k] = 0.5 * (f_deriv_um_y[k] + f_deriv_ui_y[k]);
        }
        for (int k = 0; k < 3; k++) {
          fluxL_y[k] = 0.5 * (f_um_y[k] + f_ui_y[k]) - 0.5 * fabs(aL_roe_y[k]) * (ui_y[k] - um_y[k]);
        }

        // Right interface flux: F_{j + 1/2} = 0.5 * (F(U_{j + 1}) + F(U_j)) - 0.5 * |aR_roe_y| * (U_{j + 1} - U_j).
        for (int k = 0; k < 3; k++) {
          aR_roe_y[k] = 0.5 * (f_deriv_ui_y[k] + f_deriv_up_y[k]);
        }
        for (int k = 0; k < 3; k++) {
          fluxR_y[k] = 0.5 * (f_ui_y[k] + f_up_y[k]) - 0.5 * fabs(aR_roe_y[k]) * (up_y[k] - ui_y[k]);
        }

        // Update the conserved variable vector.
        for (int k = 0; k < 3; k++) {
          un[i][(j * 3) + k] = ui_y[k] - (dt / (2.0 * dy)) * (fluxR_y[k] - fluxL_y[k]);
        }
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 1; i++) {
      for (int j = 0; j <= ny + 1; j++) {
        for (int k = 0; k < 3; k++) {
          u[i][(j * 3) + k] = un[i][(j * 3) + k];
        }
      }
    }

    // Apply simple boundary conditions in the x-direction (transmissive).
    for (int j = 0; j <= ny + 1; j++) {
      for (int k = 0; k < 3; k++) {
        u[0][(j * 3) + k] = u[1][(j * 3) + k];
        u[nx + 1][(j * 3) + k] = u[nx][(j * 3) + k];

        un[0][(j * 3) + k] = un[1][(j * 3) + k];
        un[nx + 1][(j * 3) + k] = un[nx][(j * 3) + k];
      }
    }

    // Apply simple boundary conditions in the y-direction (transmissive).
    for (int i = 0; i <= nx + 1; i++) {
      for (int k = 0; k < 3; k++) {
        u[i][(0 * 3) + k] = u[i][(1 * 3) + k];
        u[i][((ny + 1) * 3) + k] = u[i][(ny * 3) + k];

        un[i][(0 * 3) + k] = un[i][(1 * 3) + k];
        un[i][((ny + 1) * 3) + k] = un[i][(ny * 3) + k];
      }
    }

    // Output solution to disk.
    for (int k = 0; k < 3; k++) {
      const char *fmt = \"%s_output_%d_%d.csv\";
      int sz = snprintf(0, 0, fmt, \"~a\", k, n);
      char file_nm[sz + 1];
      snprintf(file_nm, sizeof file_nm, fmt, \"~a\", k, n);
    
      FILE *fptr = fopen(file_nm, \"w\");
      if (fptr != NULL) {
        for (int i = 1; i <= nx; i++) {
          for (int j = 1; j <= ny; j++) {
            double x = x0 + (i - 0.5) * dx;
            double y = y0 + (j - 0.5) * dy;
            fprintf(fptr, \"%f, %f, %f\\n\", x, y, u[i][(j * 3) + k]);
          }
        }

        fclose(fptr);
      }
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  for (int i = 0; i <= nx + 1; i++) {
    free(u[i]);
    free(un[i]);
  }
  free(u);
  free(un);

  free(local_alpha_x);
  free(local_alpha_y);
  
  free(um_x);
  free(ui_x);
  free(up_x);

  free(f_um_x);
  free(f_ui_x);
  free(f_up_x);

  free(f_deriv_um_x);
  free(f_deriv_ui_x);
  free(f_deriv_up_x);

  free(aL_roe_x);
  free(aR_roe_x);

  free(fluxL_x);
  free(fluxR_x);

  free(um_y);
  free(ui_y);
  free(up_y);

  free(f_um_y);
  free(f_ui_y);
  free(f_up_y);

  free(f_deriv_um_y);
  free(f_deriv_ui_y);
  free(f_deriv_up_y);

  free(aL_roe_y);
  free(aR_roe_y);

  free(fluxL_y);
  free(fluxR_y);
  
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
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           (list-ref init-func-codes 2)
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           (list-ref init-func-codes 2)
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals-x 0)
           (list-ref max-speed-locals-x 1)
           (list-ref max-speed-locals-x 2)
           (list-ref max-speed-locals-y 0)
           (list-ref max-speed-locals-y 1)
           (list-ref max-speed-locals-y 2)
           ;; Expressions for algebraic constraints.
           conditions-local-top
           conditions-local-bottom
           conditions-local-left
           conditions-local-right
           ;; Left, middle, right flux vectors in y-direction F(u_{j - 1}), F(u_j), F(u_{j + 1}).
           (list-ref flux-ums-y 0)
           (list-ref flux-ums-y 1)
           (list-ref flux-ums-y 2)
           (list-ref flux-uis-y 0)
           (list-ref flux-uis-y 1)
           (list-ref flux-uis-y 2)
           (list-ref flux-ups-y 0)
           (list-ref flux-ups-y 1)
           (list-ref flux-ups-y 2)
           ;; Eigenvalues of left, middle, right flux Jacobians in y-direction F'(u_{j - 1}), F'(u_j), F'(u_{j + 1}).
           (list-ref flux-deriv-ums-y 0)
           (list-ref flux-deriv-ums-y 1)
           (list-ref flux-deriv-ums-y 2)
           (list-ref flux-deriv-uis-y 0)
           (list-ref flux-deriv-uis-y 1)
           (list-ref flux-deriv-uis-y 2)
           (list-ref flux-deriv-ups-y 0)
           (list-ref flux-deriv-ups-y 1)
           (list-ref flux-deriv-ups-y 2)
           ;; Left, middle, right flux vectors in x-direction F(u_{i - 1}), F(u_i), F(u_{i + 1}).
           (list-ref flux-ums-x 0)
           (list-ref flux-ums-x 1)
           (list-ref flux-ums-x 2)
           (list-ref flux-uis-x 0)
           (list-ref flux-uis-x 1)
           (list-ref flux-uis-x 2)
           (list-ref flux-ups-x 0)
           (list-ref flux-ups-x 1)
           (list-ref flux-ups-x 2)
           ;; Eigenvalues of left, middle, right flux Jacobians in x-direction F'(u_{i - 1}), F'(u_i), F'(u_{i + 1}).
           (list-ref flux-deriv-ums-x 0)
           (list-ref flux-deriv-ums-x 1)
           (list-ref flux-deriv-ums-x 2)
           (list-ref flux-deriv-uis-x 0)
           (list-ref flux-deriv-uis-x 1)
           (list-ref flux-deriv-uis-x 2)
           (list-ref flux-deriv-ups-x 0)
           (list-ref flux-deriv-ups-x 1)
           (list-ref flux-deriv-ups-x 2)
           ;; Left, middle, right flux vectors in y-direction F(u_{j - 1}), F(u_j), F(u_{j + 1}).
           (list-ref flux-ums-y 0)
           (list-ref flux-ums-y 1)
           (list-ref flux-ums-y 2)
           (list-ref flux-uis-y 0)
           (list-ref flux-uis-y 1)
           (list-ref flux-uis-y 2)
           (list-ref flux-ups-y 0)
           (list-ref flux-ups-y 1)
           (list-ref flux-ups-y 2)
           ;; Eigenvalues of left, middle, right flux Jacobians in y-direction F'(u_{j - 1}), F'(u_j), F'(u_{j + 1}).
           (list-ref flux-deriv-ums-y 0)
           (list-ref flux-deriv-ums-y 1)
           (list-ref flux-deriv-ums-y 2)
           (list-ref flux-deriv-uis-y 0)
           (list-ref flux-deriv-uis-y 1)
           (list-ref flux-deriv-uis-y 2)
           (list-ref flux-deriv-ups-y 0)
           (list-ref flux-deriv-ups-y 1)
           (list-ref flux-deriv-ups-y 2)
           ;; PDE name for file output.
           name
           name
           ))
  code)