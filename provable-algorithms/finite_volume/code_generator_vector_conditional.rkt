#lang racket

(require "prover_core.rkt")
(require "prover_vector.rkt")
(require "code_generator_core.rkt")
(provide generate-lax-friedrichs-vector2-1d-conditional
         generate-roe-vector2-1d-conditional)

;; ---------------------------------------------------------------------------------------------------------------------------
;; Lax–Friedrichs (Finite-Difference) Solver for a 1D Coupled Vector System of 2 PDEs subject to certain algebraic constraints
;; ---------------------------------------------------------------------------------------------------------------------------
(define (generate-lax-friedrichs-vector2-1d-conditional pde-system conds epsilon
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
 "Generate C code that solves the 1D coupled vector system of 2 PDEs specified by `pde-system` using the Lax-Friedrichs finite-difference method,
  subject to the algebraic conditions `conds` with machine epsilon `epsilon`.
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

  (define epsilon-code (convert-expr epsilon))
  (define conditions-code (convert-expr (cons `and conds)))
  (define conditions-local (flux-substitute (flux-substitute (flux-substitute (flux-substitute (flux-substitute (flux-substitute
                                                                                                                 conditions-code (list-ref cons-codes 0) "u[(i * 2) + 0]")
                                                                                                                (list-ref cons-codes 1) "u[(i * 2) + 1]")
                                                                                               ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code))
                                                             "> 0.0" (string-append "> -" epsilon-code)) "> 0" (string-append "> -" epsilon-code)))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR COUPLED VECTOR PDE SYSTEM: ~a
// Lax–Friedrichs first-order finite-difference solver for a coupled vector system of 2 PDEs in 1D subject to algebraic constraints.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

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

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 2) + 1] = ~a; // init-funcs[1] in C.
    un[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    un[(i * 2) + 1] = ~a; // init-funcs[1] in C.
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

    for (int i = 0; i <= nx + 1; i++) {
      if (!(~a)) {
        printf(\"Time-step failed!\\n\");
        return 0;
      }
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
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           ;; Expression for algebraic constraints.
           conditions-local
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
           ))
  code)

;; ------------------------------------------------------------------------------------------------------------
;; Roe (Finite-Volume) Solver for a 1D Coupled Vector System of 2 PDEs subject to certain algebraic constraints
;; ------------------------------------------------------------------------------------------------------------
(define (generate-roe-vector2-1d-conditional pde-system conds epsilon
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
 "Generate C code that solves the 1D coupled vector system of 2 PDEs specified by `pde-system` using the Roe finite-volume method,
  subject to the algebraic conditions `conds` with machine epsilon `epsilon`.
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

  (define flux-jacobian-eigvals (symbolic-eigvals2 (symbolic-jacobian flux-exprs cons-exprs)))
  (define flux-jacobian-eigvals-simp (list (symbolic-simp (list-ref flux-jacobian-eigvals 0))
                                           (symbolic-simp (list-ref flux-jacobian-eigvals 1))))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define flux-codes (map (lambda (flux-expr)
                            (convert-expr flux-expr)) flux-exprs))
  (define flux-deriv-codes (map (lambda (flux-deriv-expr)
                                  (convert-expr flux-deriv-expr)) flux-jacobian-eigvals-simp))
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

  (define flux-deriv-ums (map (lambda (flux-deriv-code)
                                (flux-substitute (flux-substitute flux-deriv-code (list-ref cons-codes 0) "um[0]")
                                                 (list-ref cons-codes 1) "um[1]")) flux-deriv-codes))
  (define flux-deriv-uis (map (lambda (flux-deriv-code)
                                (flux-substitute (flux-substitute flux-deriv-code (list-ref cons-codes 0) "ui[0]")
                                                 (list-ref cons-codes 1) "ui[1]")) flux-deriv-codes))
  (define flux-deriv-ups (map (lambda (flux-deriv-code)
                                (flux-substitute (flux-substitute flux-deriv-code (list-ref cons-codes 0) "up[0]")
                                                 (list-ref cons-codes 1) "up[1]")) flux-deriv-codes))
  
  (define max-speed-locals (map (lambda (max-speed-code)
                                  (flux-substitute (flux-substitute max-speed-code (list-ref cons-codes 0) "u[(i * 2) + 0]")
                                                   (list-ref cons-codes 1) "u[(i * 2) + 1]")) max-speed-codes))

  (define cons-codes-left (list (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 0)) "L")))
                                (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 1)) "L")))))
  (define cons-codes-right (list (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 0)) "R")))
                                 (convert-expr (string->symbol (string-append (symbol->string (list-ref cons-exprs 1)) "R")))))

  (define epsilon-code (convert-expr epsilon))
  (define conditions-code (convert-expr (cons `and conds)))
  (define conditions-local-left (flux-substitute (flux-substitute (flux-substitute (flux-substitute
                                                                                    (flux-substitute (flux-substitute conditions-code (list-ref cons-codes-left 0) "u[((i - 1) * 2) + 0]")
                                                                                                     (list-ref cons-codes-left 1) "u[((i - 1) * 2) + 1]")
                                                                                    (list-ref cons-codes-right 0) "u[(i * 2) + 0]") (list-ref cons-codes-right 1) "u[(i * 2) + 1]")
                                                                  ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code)))
  (define conditions-local-right (flux-substitute (flux-substitute (flux-substitute (flux-substitute
                                                                                     (flux-substitute (flux-substitute conditions-code (list-ref cons-codes-left 0) "u[(i * 2) + 0]")
                                                                                                      (list-ref cons-codes-left 1) "u[(i * 2) + 1]")
                                                                                     (list-ref cons-codes-right 0) "u[((i + 1) * 2) + 0]") (list-ref cons-codes-right 1) "u[((i + 1) * 2) + 1]")
                                                                   ">= 0.0" (string-append ">= -" epsilon-code)) ">= 0" (string-append ">= -" epsilon-code)))
  
  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))
  
  (define code
    (format "
// AUTO-GENERATED CODE FOR COUPLED VECTOR PDE SYSTEM: ~a
// Roe higher-order finite-volume solver for a coupled vector system of 2 PDEs in 1D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

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

  double *f_deriv_um = (double*) malloc(2 * sizeof(double));
  double *f_deriv_ui = (double*) malloc(2 * sizeof(double));
  double *f_deriv_up = (double*) malloc(2 * sizeof(double));

  double *aL_roe = (double*) malloc(2 * sizeof(double));
  double *aR_roe = (double*) malloc(2 * sizeof(double));

  double *fluxL = (double*) malloc(2 * sizeof(double));
  double *fluxR = (double*) malloc(2 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 2) + 1] = ~a; // init-funcs[1] in C.
    un[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    un[(i * 2) + 1] = ~a; // init-funcs[1] in C.
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

    for (int i = 1; i <= nx; i++) {
      if (!(~a)) {
        printf(\"Time-step failed!\\n\");
        return 0;
      }

      if (!(~a)) {
        printf(\"Time-step failed!\\n\");
        return 0;
      }
    }

    // Compute fluxes with Roe approximation and update the conserved variable vector.
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

      // Evaluate eigenvalues of the flux Jacobian for each value of the conserved variable vector.
      f_deriv_um[0] = ~a;
      f_deriv_um[1] = ~a; // Eigenvalues of F'(U_{i - 1}).
      
      f_deriv_ui[0] = ~a;
      f_deriv_ui[1] = ~a; // Eigenvalues of F'(U_i).
      
      f_deriv_up[0] = ~a;
      f_deriv_up[1] = ~a; // Eigenvalues of F'(U_{i + 1}).

      // Left interface flux: F_{i - 1/2} = 0.5 * (F(U_{i - 1}) + F(U_i)) - 0.5 * |aL_roe| * (U_i - U_{i - 1}).
      for (int j = 0; j < 2; j++) {
        aL_roe[j] = 0.5 * (f_deriv_um[j] + f_deriv_ui[j]);
      }
      for (int j = 0; j < 2; j++) {
        fluxL[j] = 0.5 * (f_um[j] + f_ui[j]) - 0.5 * fabs(aL_roe[j]) * (ui[j] - um[j]);
      }

      // Right interface flux: F_{i + 1/2} = 0.5 * (F(U_{i + 1}) + F(U_i)) - 0.5 * |aR_roe| * (U_{i + 1} - u_i).
      for (int j = 0; j < 2; j++) {
        aR_roe[j] = 0.5 * (f_deriv_ui[j] + f_deriv_up[j]);
      }
      for (int j = 0; j < 2; j++) {
        fluxR[j] = 0.5 * (f_ui[j] + f_up[j]) - 0.5 * fabs(aR_roe[j]) * (up[j] - ui[j]);
      }

      // Update the conserved variable.
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

  free(u);
  free(un);

  free(local_alpha);
  
  free(um);
  free(ui);
  free(up);

  free(f_um);
  free(f_ui);
  free(f_up);

  free(f_deriv_um);
  free(f_deriv_ui);
  free(f_deriv_up);

  free(aL_roe);
  free(aR_roe);

  free(fluxL);
  free(fluxR);
  
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
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           ;; Expressions for algebraic constraints.
           conditions-local-left
           conditions-local-right
           ;; Left flux vector F(u_{i - 1}).
           (list-ref flux-ums 0)
           (list-ref flux-ums 1)
           ;; Middle flux vector F(u_i).
           (list-ref flux-uis 0)
           (list-ref flux-uis 1)
           ;; Right flux vector F(u_{i + 1}).
           (list-ref flux-ups 0)
           (list-ref flux-ups 1)
           ;; Eigenvalues of left flux Jacobian F'(u_{i - 1}).
           (list-ref flux-deriv-ums 0)
           (list-ref flux-deriv-ums 1)
           ;; Eigenvalues of middle flux Jacobian F'(u_i).
           (list-ref flux-deriv-uis 0)
           (list-ref flux-deriv-uis 1)
           ;; Eigenvalues of right flux Jacobian F'(u_{i + 1}).
           (list-ref flux-deriv-ups 0)
           (list-ref flux-deriv-ups 1)
           ;; PDE name for file output.
           name
           name
           ))
  code)