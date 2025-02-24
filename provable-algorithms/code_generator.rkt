#lang racket

(require "prover.rkt")
(provide convert-expr
         remove-bracketed-expressions
         remove-bracketed-expressions-from-file
         flux-substitute
         generate-lax-friedrichs-scalar-1d
         generate-lax-friedrichs-scalar-1d-second-order
         generate-roe-scalar-1d
         generate-roe-scalar-1d-second-order
         generate-lax-friedrichs-vector2-1d
         generate-lax-friedrichs-vector2-1d-second-order
         generate-roe-vector2-1d
         generate-roe-vector2-1d-second-order)

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

    ;; If expr is a maximum of the form (max expr1 expr2), then convert it to "fmax(expr1, expr2)" in C.
    [`(max ,arg1 ,arg2)
     (format "fmax(~a, ~a)" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a maximum of the form (max expr1 expr2 expr2), then convert it to "fmax(expr1, expr2, expr3)" in C.
    [`(max ,arg1 ,arg2 ,arg3)
     (format "fmax(~a, ~a, ~a)" (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    ;; If expr is a minimum of the form (max expr1 expr2), then convert it to "fmin(expr1, expr2)" in C.
    [`(min ,arg1 ,arg2)
     (format "fmin(~a, ~a)" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a minimum of the form (max expr1 expr2 expr2), then convert it to "fmin(expr1, expr2, expr3)" in C.
    [`(min ,arg1 ,arg2 ,arg3)
     (format "fmin(~a, ~a, ~a)" (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

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

;; -------------------------------------------------------------
;; Lax–Friedrichs (Finite-Difference) Solver for a 1D Scalar PDE
;; -------------------------------------------------------------
(define (generate-lax-friedrichs-scalar-1d pde
                                           #:nx [nx 200]
                                           #:x0 [x0 0.0]
                                           #:x1 [x1 2.0]
                                           #:t-final [t-final 1.0]
                                           #:cfl [cfl 0.95]
                                           #:init-func [init-func `(cond
                                                                     [(< x 1.0) 1.0]
                                                                     [else 0.0])])
 "Generate C code that solves the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method.
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
// AUTO-GENERATED CODE FOR SCALAR PDE: ~a
// Lax–Friedrichs first-order finite-difference solver for a scalar PDE in 1D.

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
  double *u = (double*) malloc((nx + 2) * sizeof(double));
  double *un = (double*) malloc((nx + 2) * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  double t = 0.0;
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

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 1; i <= nx; i++) {
    double x = x0 + (i - 0.5) * dx;
    printf(\"%g %g\\n\", x, u[i]);
  }

  free(u);
  free(un);
   
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
           ))
  code)

;; ----------------------------------------------------------------------------------------------------
;; Lax–Friedrichs (Finite-Difference) Solver for a 1D Scalar PDE with a Second-Order Flux Extrapolation
;; ----------------------------------------------------------------------------------------------------
(define (generate-lax-friedrichs-scalar-1d-second-order pde limiter
                                                        #:nx [nx 200]
                                                        #:x0 [x0 0.0]
                                                        #:x1 [x1 2.0]
                                                        #:t-final [t-final 1.0]
                                                        #:cfl [cfl 0.95]
                                                        #:init-func [init-func `(cond
                                                                                  [(< x 1.0) 1.0]
                                                                                  [else 0.0])])
 "Generate C code that solves the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method with a second-order flux extrapolation using flux limiter `limiter`.
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
// AUTO-GENERATED CODE FOR SCALAR PDE: ~a
// FLUX LIMITER: ~a
// Lax–Friedrichs first-order finite-difference solver for a scalar PDE in 1D, with a second-order flux extrapolation.

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

  // Array for storing slopes.
  double *slope = (double*) malloc((nx + 4) * sizeof(double));

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 4) * sizeof(double));
  double *un = (double*) malloc((nx + 4) * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 3; i++) {
    double x = x0 + (i - 1.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  double t = 0.0;
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

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 2; i <= nx + 1; i++) {
    double x = x0 + (i - 1.5) * dx;
    printf(\"%g %g\\n\", x, u[i]);
  }

  free(u);
  free(un);
  free(slope);
   
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
           ))
  code)

;; ----------------------------------------------
;; Roe (Finite-Volume) Solver for a 1D Scalar PDE
;; ----------------------------------------------
(define (generate-roe-scalar-1d pde
                                #:nx [nx 200]
                                #:x0 [x0 0.0]
                                #:x1 [x1 2.0]
                                #:t-final [t-final 1.0]
                                #:cfl [cfl 0.95]
                                #:init-func [init-func `(cond
                                                          [(< x 1.0) 1.0]
                                                          [else 0.0])])
 "Generate C code that solves the 1D scalar PDE specified by `pde` using the Roe finite-volume method.
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

  (define flux-deriv (symbolic-simp (symbolic-diff flux-expr cons-expr)))

  (define cons-code (convert-expr cons-expr))
  (define flux-code (convert-expr flux-expr))
  (define flux-deriv-code (convert-expr flux-deriv))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))

  (define flux-um (flux-substitute flux-code cons-code "um"))
  (define flux-ui (flux-substitute flux-code cons-code "ui"))
  (define flux-up (flux-substitute flux-code cons-code "up"))

  (define flux-deriv-um (flux-substitute flux-deriv-code cons-code "um"))
  (define flux-deriv-ui (flux-substitute flux-deriv-code cons-code "ui"))
  (define flux-deriv-up (flux-substitute flux-deriv-code cons-code "up"))

  (define max-speed-local (flux-substitute max-speed-code cons-code "u[i]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR SCALAR PDE: ~a
// Roe higher-order finite-volume solver for a scalar PDE in 1D.

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
  double *u = (double*) malloc((nx + 2) * sizeof(double));
  double *un = (double*) malloc((nx + 2) * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  double t = 0.0;
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

    // Compute fluxes with Roe approximation and update the conserved variable.
    for (int i = 1; i <= nx; i++) {
      double um = u[i - 1];
      double ui = u[i];
      double up = u[i + 1];

      // Evaluate flux for each value of the conserved variable.
      double f_um = ~a; // f(u_{i - 1}).
      double f_ui = ~a; // f(u_i).
      double f_up = ~a; // f(u_{i + 1}).

      // Evaluate flux derivative for each value of the conserved variable.
      double f_deriv_um = ~a; // f'(u_{i - 1}).
      double f_deriv_ui = ~a; // f'(u_i).
      double f_deriv_up = ~a; // f'(u_{i + 1}).

      // Left interface flux: F_{i - 1/2} = 0.5 * (f(u_{i - 1}) + f(u_i)) - 0.5 * |aL_roe| * (u_i - u_{i - 1}).
      double aL_roe = 0.5 * (f_deriv_um + f_deriv_ui);
      double fluxL = 0.5 * (f_um + f_ui) - 0.5 * fabs(aL_roe) * (ui - um);

      // Right interface flux: F_{i + 1/2} = 0.5 * (f(u_{i + 1}) + f(u_i)) - 0.5 * |aR_roe| * (u_{i + 1} - u_i).
      double aR_roe = 0.5 * (f_deriv_ui + f_deriv_up);
      double fluxR = 0.5 * (f_ui + f_up) - 0.5 * fabs(aR_roe) * (up - ui);

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

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 1; i <= nx; i++) {
    double x = x0 + (i - 0.5) * dx;
    printf(\"%g %g\\n\", x, u[i]);
  }

  free(u);
  free(un);
   
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
           ;; Left flux derivative f'(u_{i - 1}).
           flux-deriv-um
           ;; Middle flux derivative f'(u_i).
           flux-deriv-ui
           ;; Right flux derivative f'(u_{i + 1}).
           flux-deriv-up
           ))
  code)

;; -------------------------------------------------------------------------------------
;; Roe (Finite-Volume) Solver for a 1D Scalar PDE with a Second-Order Flux Extrapolation
;; -------------------------------------------------------------------------------------
(define (generate-roe-scalar-1d-second-order pde limiter
                                             #:nx [nx 200]
                                             #:x0 [x0 0.0]
                                             #:x1 [x1 2.0]
                                             #:t-final [t-final 1.0]
                                             #:cfl [cfl 0.95]
                                             #:init-func [init-func `(cond
                                                                       [(< x 1.0) 1.0]
                                                                       [else 0.0])])
 "Generate C code that solves the 1D scalar PDE specified by `pde` using the Roe finite-volume method with a second-order flux extrapolation using flux limiter `limiter`.
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

  (define flux-deriv (symbolic-simp (symbolic-diff flux-expr cons-expr)))

  (define cons-code (convert-expr cons-expr))
  (define flux-code (convert-expr flux-expr))
  (define flux-deriv-code (convert-expr flux-deriv))
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

  (define flux-deriv-umR-evol (flux-substitute flux-deriv-code cons-code "umR_evol"))
  (define flux-deriv-uiL-evol (flux-substitute flux-deriv-code cons-code "uiL_evol"))
  (define flux-deriv-uiR-evol (flux-substitute flux-deriv-code cons-code "uiR_evol"))
  (define flux-deriv-upL-evol (flux-substitute flux-deriv-code cons-code "upL_evol"))

  (define max-speed-local (flux-substitute max-speed-code cons-code "u[i]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR SCALAR PDE: ~a
// FLUX LIMITER: ~a
// Roe higher-order finite-volume solver for a scalar PDE in 1D, with a second-order flux extrapolation.

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

  // Array for storing slopes.
  double *slope = (double*) malloc((nx + 4) * sizeof(double));
  
  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 4) * sizeof(double));
  double *un = (double*) malloc((nx + 4) * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 3; i++) {
    double x = x0 + (i - 1.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  double t = 0.0;
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

    // Compute fluxes with Roe approximation and update the conserved variable.
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

      // Evaluate flux derivative for each value of the (evolved) conserved variable.
      double f_deriv_umR_evol = ~a;
      double f_deriv_uiL_evol = ~a;

      double f_deriv_uiR_evol = ~a;
      double f_deriv_upL_evol = ~a;

      // Left interface flux: F_{i - 1/2} = 0.5 * (f(u_{i - 1, R+}) + f(u_{i, L+})) - 0.5 * |aL_roe| * (u_{i, L+} - u_{i - 1, R+}).
      double aL_roe = 0.5 * (f_deriv_umR_evol + f_deriv_uiL_evol);
      double fluxL = 0.5 * (f_umR_evol + f_uiL_evol) - 0.5 * fabs(aL_roe) * (uiL_evol - umR_evol);

      // Right interface flux: F_{i + 1/2} = 0.5 * (f(u_{i + 1, L+}) + f(u_{i, R+})) - 0.5 * |aR_roe| * (u_{i + 1, L+} - u_{i, R+}).
      double aR_roe = 0.5 * (f_deriv_uiR_evol + f_deriv_upL_evol);
      double fluxR = 0.5 * (f_uiR_evol + f_upL_evol) - 0.5 * fabs(aR_roe) * (upL_evol - uiR_evol);

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

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 2; i <= nx + 1; i++) {
    double x = x0 + (i - 1.5) * dx;
    printf(\"%g %g\\n\", x, u[i]);
  }

  free(u);
  free(un);
  free(slope);
   
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
           ;; Evolved right negative flux derivative f'(u_{i - 1, R+}).
           flux-deriv-umR-evol
           ;; Evolved left central flux derivative f'(u_{i, L+}).
           flux-deriv-uiL-evol
           ;; Evolved right central flux derivative f'(u_{i, R+}).
           flux-deriv-uiR-evol
           ;; Evolved left positive flux derivative f'(u_{i + 1, L+}).
           flux-deriv-upL-evol
           ))
  code)

;; ----------------------------------------------------------------------------------
;; Lax–Friedrichs (Finite-Difference) Solver for a 1D Coupled Vector System of 2 PDEs
;; ----------------------------------------------------------------------------------
(define (generate-lax-friedrichs-vector2-1d pde-system
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
 "Generate C code that solves the 1D coupled vector system of 2 PDEs specified by `pde-system` using the Lax-Friedrichs finite-difference method.
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

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR COUPLED VECTOR PDE SYSTEM: ~a
// Lax–Friedrichs first-order finite-difference solver for a coupled vector system of 2 PDEs in 1D.

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
  }

  double t = 0.0;
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

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 1; i <= nx; i++) {
    double x = x0 + (i - 0.5) * dx;
    printf(\"%g %g %g\\n\", x, u[(i * 2) + 0], u[(i * 2) + 1]);
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
           ))
  code)

;; -------------------------------------------------------------------------------------------------------------------------
;; Lax–Friedrichs (Finite-Difference) Solver for a 1D Coupled Vector System of 2 PDEs with a Second-Order Flux Extrapolation
;; -------------------------------------------------------------------------------------------------------------------------
(define (generate-lax-friedrichs-vector2-1d-second-order pde-system limiter
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
 "Generate C code that solves the 1D coupled vector system of 2 PDEs specified by `pde-system` using the Lax-Friedrichs finite-difference method with a
  second-order flux extrapolation using flux limiter `limiter`.
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

  (define limiter-name (hash-ref limiter 'name))
  (define limiter-expr (hash-ref limiter 'limiter-expr))
  (define limiter-ratio (hash-ref limiter 'limiter-ratio))

  (define limiter-code (convert-expr limiter-expr))
  (define limiter-ratio-code (convert-expr limiter-ratio))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define flux-codes (map (lambda (flux-expr)
                            (convert-expr flux-expr)) flux-exprs))
  (define max-speed-codes (map (lambda (max-speed-expr)
                                 (convert-expr max-speed-expr)) max-speed-exprs))
  (define init-func-codes (map (lambda (init-func-expr)
                                 (convert-expr init-func-expr)) init-funcs))

  (define limiter-r (flux-substitute limiter-code limiter-ratio-code "r"))

  (define flux-umLs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "umL[0]")
                                            (list-ref cons-codes 1) "umL[1]")) flux-codes))
  (define flux-umRs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "umR[0]")
                                            (list-ref cons-codes 1) "umR[1]")) flux-codes))
  (define flux-uiLs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiL[0]")
                                            (list-ref cons-codes 1) "uiL[1]")) flux-codes))
  (define flux-uiRs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiR[0]")
                                            (list-ref cons-codes 1) "uiR[1]")) flux-codes))
  (define flux-upLs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "upL[0]")
                                            (list-ref cons-codes 1) "upL[1]")) flux-codes))
  (define flux-upRs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "upR[0]")
                                            (list-ref cons-codes 1) "upR[1]")) flux-codes))
  
  (define flux-umR-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "umR_evol[0]")
                                                 (list-ref cons-codes 1) "umR_evol[1]")) flux-codes))
  (define flux-uiL-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiL_evol[0]")
                                                 (list-ref cons-codes 1) "uiL_evol[1]")) flux-codes))
  (define flux-uiR-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiR_evol[0]")
                                                 (list-ref cons-codes 1) "uiR_evol[1]")) flux-codes))
  (define flux-upL-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "upL_evol[0]")
                                                 (list-ref cons-codes 1) "upL_evol[1]")) flux-codes))
  
  (define max-speed-locals (map (lambda (max-speed-code)
                                  (flux-substitute (flux-substitute max-speed-code (list-ref cons-codes 0) "u[(i * 2) + 0]")
                                                   (list-ref cons-codes 1) "u[(i * 2) + 1]")) max-speed-codes))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR COUPLED VECTOR PDE SYSTEM: ~a
// FLUX LIMITER: ~a
// Lax–Friedrichs first-order finite-difference solver for a coupled vector system of 2 PDEs in 1D, with a second-order flux extrapolation.

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

  // Array for storing slopes.
  double *slope = (double*) malloc((nx + 4) * 2 * sizeof(double));

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 4) * 2 * sizeof(double));
  double *un = (double*) malloc((nx + 4) * 2 * sizeof(double));

  // Arrays for storing other intermediate values.
  double *local_alpha = (double*) malloc(2 * sizeof(double));
  
  double *umL = (double*) malloc(2 * sizeof(double));
  double *umR = (double*) malloc(2 * sizeof(double));
  double *uiL = (double*) malloc(2 * sizeof(double));
  double *uiR = (double*) malloc(2 * sizeof(double));
  double *upL = (double*) malloc(2 * sizeof(double));
  double *upR = (double*) malloc(2 * sizeof(double));

  double *f_umL = (double*) malloc(2 * sizeof(double));
  double *f_umR = (double*) malloc(2 * sizeof(double));
  double *f_uiL = (double*) malloc(2 * sizeof(double));
  double *f_uiR = (double*) malloc(2 * sizeof(double));
  double *f_upL = (double*) malloc(2 * sizeof(double));
  double *f_upR = (double*) malloc(2 * sizeof(double));

  double *umR_evol = (double*) malloc(2 * sizeof(double));
  double *uiL_evol = (double*) malloc(2 * sizeof(double));
  double *uiR_evol = (double*) malloc(2 * sizeof(double));
  double *upL_evol = (double*) malloc(2 * sizeof(double));

  double *f_umR_evol = (double*) malloc(2 * sizeof(double));
  double *f_uiL_evol = (double*) malloc(2 * sizeof(double));
  double *f_uiR_evol = (double*) malloc(2 * sizeof(double));
  double *f_upL_evol = (double*) malloc(2 * sizeof(double));

  double *fluxL = (double*) malloc(2 * sizeof(double));
  double *fluxR = (double*) malloc(2 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 3; i++) {
    double x = x0 + (i - 1.5) * dx;
    
    u[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 2) + 1] = ~a; // init-funcs[1] in C.
  }

  double t = 0.0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 2; i <= nx + 1; i++) {
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

    // Compute appropriately flux-limited slopes within each cell.
    for (int i = 1; i <= nx + 2; i++) {
      for (int j = 0; j < 2; j++) {
        double r = (u[(i * 2) + j] - u[((i - 1) * 2) + j]) / (u[((i + 1) * 2) + j] - u[(i * 2) + j]);
        double limiter = ~a; // limiter-r in C.

        slope[(i * 2) + j] = limiter * (0.5 * ((u[(i * 2) + j] - u[((i - 1) * 2) + j]) + (u[((i + 1) * 2) + j] - u[(i * 2) + j])));
      }
    }

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable vector.
    for (int i = 2; i <= nx + 1; i++) {
      // Extrapolate boundary states.
      for (int j = 0; j < 2; j++) {
        umL[j] = u[((i - 1) * 2) + j] - (0.5 * slope[((i - 1) * 2) + j]);
        umR[j] = u[((i - 1) * 2) + j] + (0.5 * slope[((i - 1) * 2) + j]);

        uiL[j] = u[(i * 2) + j] - (0.5 * slope[(i * 2) + j]);
        uiR[j] = u[(i * 2) + j] + (0.5 * slope[(i * 2) + j]);

        upL[j] = u[((i + 1) * 2) + j] - (0.5 * slope[((i + 1) * 2) + j]);
        upR[j] = u[((i + 1) * 2) + j] + (0.5 * slope[((i + 1) * 2) + j]);
      }

      // Evaluate flux vector for each extrapolated boundary state.
      f_umL[0] = ~a;
      f_umL[1] = ~a;
      f_umR[0] = ~a;
      f_umR[1] = ~a;

      f_uiL[0] = ~a;
      f_uiL[1] = ~a;
      f_uiR[0] = ~a;
      f_uiR[1] = ~a;

      f_upL[0] = ~a;
      f_upL[1] = ~a;
      f_upR[0] = ~a;
      f_upR[1] = ~a;

      // Evolve each extrapolated boundary state.
      for (int j = 0; j < 2; j++) {
        umR_evol[j] = umR[j] + ((dt / (2.0 * dx)) * (f_umL[j] - f_umR[j]));

        uiL_evol[j] = uiL[j] + ((dt / (2.0 * dx)) * (f_uiL[j] - f_uiR[j]));
        uiR_evol[j] = uiR[j] + ((dt / (2.0 * dx)) * (f_uiL[j] - f_uiR[j]));

        upL_evol[j] = upL[j] + ((dt / (2.0 * dx)) * (f_upL[j] - f_upR[j]));
      }

      // Evaluate flux vector for each value of the (evolved) conserved variable vector.
      f_umR_evol[0] = ~a;
      f_umR_evol[1] = ~a; // F(U_{i - 1, R+})
      f_uiL_evol[0] = ~a;
      f_uiL_evol[1] = ~a; // F(U_{i, L+})
      
      f_uiR_evol[0] = ~a;
      f_uiR_evol[1] = ~a; // F(U_{i, R+})
      f_upL_evol[0] = ~a;
      f_upL_evol[1] = ~a; // F(U_{i + 1, L+})

      // Left interface flux: F_{i - 1/2} = 0.5 * (F(U_{i - 1, R+}) + F(U_{i, L+})) - 0.5 * alpha * (U_{i, L+} - U_{i - 1, R+}).
      for (int j = 0; j < 2; j++) {
        fluxL[j] = 0.5 * (f_umR_evol[j] + f_uiL_evol[j]) - 0.5 * alpha * (uiL_evol[j] - umR_evol[j]);
      }

      // Right interface flux: F_{i + 1/2} = 0.5 * (F(U_{i + 1, L+}) + F(U_{i, R+})) - 0.5 * alpha * (U_{i + 1, L+} - U_{i, R+}).
      for (int j = 0; j < 2; j++) {
        fluxR[j] = 0.5 * (f_uiR_evol[j] + f_upL_evol[j]) - 0.5 * alpha * (upL_evol[j] - uiR_evol[j]);
      }

      // Update the conserved variable vector.
      for (int j = 0; j < 2; j++) {
        un[(i * 2) + j] = u[(i * 2) + j] - (dt / dx) * (fluxR[j] - fluxL[j]);
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 3; i++) {
      for (int j = 0; j < 2; j++) {
        u[(i * 2) + j] = un[(i * 2) + j];
      }
    }

    // Apply simple boundary conditions (transmissive).
    for (int j = 0; j < 2; j++) {
      u[(0 * 2) + j] = u[(2 * 2) + j];
      u[(1 * 2) + j] = u[(2 * 2) + j];
      u[((nx + 2) * 2) + j] = u[((nx + 1) * 2) + j];
      u[((nx + 3) * 2) + j] = u[((nx + 1) * 2) + j];
    }

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 2; i <= nx + 1; i++) {
    double x = x0 + (i - 1.5) * dx;
    printf(\"%g %g %g\\n\", x, u[(i * 2) + 0], u[(i * 2) + 1]);
  }

  free(u);
  free(un);
  free(slope);

  free(local_alpha);
  
  free(umL);
  free(umR);
  free(uiL);
  free(uiR);
  free(upL);
  free(upR);

  free(f_umL);
  free(f_umR);
  free(f_uiL);
  free(f_uiR);
  free(f_upL);
  free(f_upR);

  free(umR_evol);
  free(uiL_evol);
  free(uiR_evol);
  free(upL_evol);

  free(f_umR_evol);
  free(f_uiL_evol);
  free(f_uiR_evol);
  free(f_upL_evol);

  free(fluxL);
  free(fluxR);
  
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
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           ;; Expression for flux limiter function.
           limiter-r
           ;; Left negative flux vector F(U_{i - 1, L}).
           (list-ref flux-umLs 0)
           (list-ref flux-umLs 1)
           ;; Right negative flux vector F(U_{i - 1, R}).
           (list-ref flux-umRs 0)
           (list-ref flux-umRs 1)
           ;; Left central flux vector F(U_{i, L}).
           (list-ref flux-uiLs 0)
           (list-ref flux-uiLs 1)
           ;; Right central flux vector F(U_{i, R}).
           (list-ref flux-uiRs 0)
           (list-ref flux-uiRs 1)
           ;; Left positive flux vector F(U_{i + 1, L}).
           (list-ref flux-upLs 0)
           (list-ref flux-upLs 1)
           ;; Right positive flux vector F(U_{i + 1, R}).
           (list-ref flux-upRs 0)
           (list-ref flux-upRs 1)
           ;; Evolved right negative flux vector F(U_{i - 1, R+}).
           (list-ref flux-umR-evols 0)
           (list-ref flux-umR-evols 1)
           ;; Evolved left central flux vector F(U_{i, L+}).
           (list-ref flux-uiL-evols 0)
           (list-ref flux-uiL-evols 1)
           ;; Evolved right central flux vector F(U_{i, R+}).
           (list-ref flux-uiR-evols 0)
           (list-ref flux-uiR-evols 1)
           ;; Evolved left positive flux vector F(U_{i + 1, L+}).
           (list-ref flux-upL-evols 0)
           (list-ref flux-upL-evols 1)
           ))
  code)

;; -------------------------------------------------------------------
;; Roe (Finite-Volume) Solver for a 1D Coupled Vector System of 2 PDEs
;; -------------------------------------------------------------------
(define (generate-roe-vector2-1d pde-system
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
 "Generate C code that solves the 1D coupled vector system of 2 PDEs specified by `pde-system` using the Roe finite-volume method.
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
  }

  double t = 0.0;
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

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 1; i <= nx; i++) {
    double x = x0 + (i - 0.5) * dx;
    printf(\"%g %g %g\\n\", x, u[(i * 2) + 0], u[(i * 2) + 1]);
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
           ;; Eigenvalues of left flux Jacobian F'(u_{i - 1}).
           (list-ref flux-deriv-ums 0)
           (list-ref flux-deriv-ums 1)
           ;; Eigenvalues of middle flux Jacobian F'(u_i).
           (list-ref flux-deriv-uis 0)
           (list-ref flux-deriv-uis 1)
           ;; Eigenvalues of right flux Jacobian F'(u_{i + 1}).
           (list-ref flux-deriv-ups 0)
           (list-ref flux-deriv-ups 1)
           ))
  code)

;; ----------------------------------------------------------------------------------------------------------
;; Roe (Finite-Volume) Solver for a 1D Coupled Vector System of 2 PDEs with a Second-Order Flux Extrapolation
;; ----------------------------------------------------------------------------------------------------------
(define (generate-roe-vector2-1d-second-order pde-system limiter
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
 "Generate C code that solves the 1D coupled vector system of 2 PDEs specified by `pde-system` using the Roe finite-volume method with a
  second-order flux extrapolation using flux limiter `limiter`.
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

  (define limiter-name (hash-ref limiter 'name))
  (define limiter-expr (hash-ref limiter 'limiter-expr))
  (define limiter-ratio (hash-ref limiter 'limiter-ratio))

  (define limiter-code (convert-expr limiter-expr))
  (define limiter-ratio-code (convert-expr limiter-ratio))

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

  (define limiter-r (flux-substitute limiter-code limiter-ratio-code "r"))

  (define flux-umLs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "umL[0]")
                                            (list-ref cons-codes 1) "umL[1]")) flux-codes))
  (define flux-umRs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "umR[0]")
                                            (list-ref cons-codes 1) "umR[1]")) flux-codes))
  (define flux-uiLs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiL[0]")
                                            (list-ref cons-codes 1) "uiL[1]")) flux-codes))
  (define flux-uiRs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiR[0]")
                                            (list-ref cons-codes 1) "uiR[1]")) flux-codes))
  (define flux-upLs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "upL[0]")
                                            (list-ref cons-codes 1) "upL[1]")) flux-codes))
  (define flux-upRs (map (lambda (flux-code)
                           (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "upR[0]")
                                            (list-ref cons-codes 1) "upR[1]")) flux-codes))
  
  (define flux-umR-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "umR_evol[0]")
                                                 (list-ref cons-codes 1) "umR_evol[1]")) flux-codes))
  (define flux-uiL-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiL_evol[0]")
                                                 (list-ref cons-codes 1) "uiL_evol[1]")) flux-codes))
  (define flux-uiR-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "uiR_evol[0]")
                                                 (list-ref cons-codes 1) "uiR_evol[1]")) flux-codes))
  (define flux-upL-evols (map (lambda (flux-code)
                                (flux-substitute (flux-substitute flux-code (list-ref cons-codes 0) "upL_evol[0]")
                                                 (list-ref cons-codes 1) "upL_evol[1]")) flux-codes))

  (define flux-deriv-umR-evols (map (lambda (flux-deriv-code)
                                      (flux-substitute (flux-substitute flux-deriv-code (list-ref cons-codes 0) "umR_evol[0]")
                                                       (list-ref cons-codes 1) "umR_evol[1]")) flux-deriv-codes))
  (define flux-deriv-uiL-evols (map (lambda (flux-deriv-code)
                                      (flux-substitute (flux-substitute flux-deriv-code (list-ref cons-codes 0) "uiL_evol[0]")
                                                       (list-ref cons-codes 1) "uiL_evol[1]")) flux-deriv-codes))
  (define flux-deriv-uiR-evols (map (lambda (flux-deriv-code)
                                      (flux-substitute (flux-substitute flux-deriv-code (list-ref cons-codes 0) "uiR_evol[0]")
                                                       (list-ref cons-codes 1) "uiR_evol[1]")) flux-deriv-codes))
  (define flux-deriv-upL-evols (map (lambda (flux-deriv-code)
                                      (flux-substitute (flux-substitute flux-deriv-code (list-ref cons-codes 0) "upL_evol[0]")
                                                       (list-ref cons-codes 1) "upL_evol[1]")) flux-deriv-codes))
  
  (define max-speed-locals (map (lambda (max-speed-code)
                                  (flux-substitute (flux-substitute max-speed-code (list-ref cons-codes 0) "u[(i * 2) + 0]")
                                                   (list-ref cons-codes 1) "u[(i * 2) + 1]")) max-speed-codes))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))
  
  (define code
    (format "
// AUTO-GENERATED CODE FOR COUPLED VECTOR PDE SYSTEM: ~a
// FLUX LIMITER: ~a
// Roe higher-order finite-volume solver for a coupled vector system of 2 PDEs in 1D, with a second-order flux extrapolation.

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

  // Array for storing slopes.
  double *slope = (double*) malloc((nx + 4) * 2 * sizeof(double));

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 4) * 2 * sizeof(double));
  double *un = (double*) malloc((nx + 4) * 2 * sizeof(double));

  // Arrays for storing other intermediate values.
  double *local_alpha = (double*) malloc(2 * sizeof(double));
  
  double *umL = (double*) malloc(2 * sizeof(double));
  double *umR = (double*) malloc(2 * sizeof(double));
  double *uiL = (double*) malloc(2 * sizeof(double));
  double *uiR = (double*) malloc(2 * sizeof(double));
  double *upL = (double*) malloc(2 * sizeof(double));
  double *upR = (double*) malloc(2 * sizeof(double));

  double *f_umL = (double*) malloc(2 * sizeof(double));
  double *f_umR = (double*) malloc(2 * sizeof(double));
  double *f_uiL = (double*) malloc(2 * sizeof(double));
  double *f_uiR = (double*) malloc(2 * sizeof(double));
  double *f_upL = (double*) malloc(2 * sizeof(double));
  double *f_upR = (double*) malloc(2 * sizeof(double));

  double *umR_evol = (double*) malloc(2 * sizeof(double));
  double *uiL_evol = (double*) malloc(2 * sizeof(double));
  double *uiR_evol = (double*) malloc(2 * sizeof(double));
  double *upL_evol = (double*) malloc(2 * sizeof(double));

  double *f_umR_evol = (double*) malloc(2 * sizeof(double));
  double *f_uiL_evol = (double*) malloc(2 * sizeof(double));
  double *f_uiR_evol = (double*) malloc(2 * sizeof(double));
  double *f_upL_evol = (double*) malloc(2 * sizeof(double));

  double *f_deriv_umR_evol = (double*) malloc(2 * sizeof(double));
  double *f_deriv_uiL_evol = (double*) malloc(2 * sizeof(double));
  double *f_deriv_uiR_evol = (double*) malloc(2 * sizeof(double));
  double *f_deriv_upL_evol = (double*) malloc(2 * sizeof(double));

  double *aL_roe = (double*) malloc(2 * sizeof(double));
  double *aR_roe = (double*) malloc(2 * sizeof(double));

  double *fluxL = (double*) malloc(2 * sizeof(double));
  double *fluxR = (double*) malloc(2 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 3; i++) {
    double x = x0 + (i - 1.5) * dx;
    
    u[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 2) + 1] = ~a; // init-funcs[1] in C.
  }

  double t = 0.0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 2; i <= nx + 1; i++) {
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

    // Compute appropriately flux-limited slopes within each cell.
    for (int i = 1; i <= nx + 2; i++) {
      for (int j = 0; j < 2; j++) {
        double r = (u[(i * 2) + j] - u[((i - 1) * 2) + j]) / (u[((i + 1) * 2) + j] - u[(i * 2) + j]);
        double limiter = ~a; // limiter-r in C.

        slope[(i * 2) + j] = limiter * (0.5 * ((u[(i * 2) + j] - u[((i - 1) * 2) + j]) + (u[((i + 1) * 2) + j] - u[(i * 2) + j])));
      }
    }

    // Compute fluxes with Roe approximation and update the conserved variable vector.
    for (int i = 2; i <= nx + 1; i++) {
      // Extrapolate boundary states.
      for (int j = 0; j < 2; j++) {
        umL[j] = u[((i - 1) * 2) + j] - (0.5 * slope[((i - 1) * 2) + j]);
        umR[j] = u[((i - 1) * 2) + j] + (0.5 * slope[((i - 1) * 2) + j]);

        uiL[j] = u[(i * 2) + j] - (0.5 * slope[(i * 2) + j]);
        uiR[j] = u[(i * 2) + j] + (0.5 * slope[(i * 2) + j]);

        upL[j] = u[((i + 1) * 2) + j] - (0.5 * slope[((i + 1) * 2) + j]);
        upR[j] = u[((i + 1) * 2) + j] + (0.5 * slope[((i + 1) * 2) + j]);
      }

      // Evaluate flux vector for each extrapolated boundary state.
      f_umL[0] = ~a;
      f_umL[1] = ~a;
      f_umR[0] = ~a;
      f_umR[1] = ~a;

      f_uiL[0] = ~a;
      f_uiL[1] = ~a;
      f_uiR[0] = ~a;
      f_uiR[1] = ~a;

      f_upL[0] = ~a;
      f_upL[1] = ~a;
      f_upR[0] = ~a;
      f_upR[1] = ~a;

      // Evolve each extrapolated boundary state.
      for (int j = 0; j < 2; j++) {
        umR_evol[j] = umR[j] + ((dt / (2.0 * dx)) * (f_umL[j] - f_umR[j]));

        uiL_evol[j] = uiL[j] + ((dt / (2.0 * dx)) * (f_uiL[j] - f_uiR[j]));
        uiR_evol[j] = uiR[j] + ((dt / (2.0 * dx)) * (f_uiL[j] - f_uiR[j]));

        upL_evol[j] = upL[j] + ((dt / (2.0 * dx)) * (f_upL[j] - f_upR[j]));
      }

      // Evaluate flux vector for each value of the (evolved) conserved variable vector.
      f_umR_evol[0] = ~a;
      f_umR_evol[1] = ~a; // F(U_{i - 1, R+})
      f_uiL_evol[0] = ~a;
      f_uiL_evol[1] = ~a; // F(U_{i, L+})
      
      f_uiR_evol[0] = ~a;
      f_uiR_evol[1] = ~a; // F(U_{i, R+})
      f_upL_evol[0] = ~a;
      f_upL_evol[1] = ~a; // F(U_{i + 1, L+})

      // Evaluate eigenvalues of the flux Jacobian for each value of the (evolved) conserved variable vector.
      f_deriv_umR_evol[0] = ~a;
      f_deriv_umR_evol[1] = ~a; // F'(U_{i - 1, R+})
      f_deriv_uiL_evol[0] = ~a;
      f_deriv_uiL_evol[1] = ~a; // F'(U_{i, L+})
      
      f_deriv_uiR_evol[0] = ~a;
      f_deriv_uiR_evol[1] = ~a; // F'(U_{i, R+})
      f_deriv_upL_evol[0] = ~a;
      f_deriv_upL_evol[1] = ~a; // F'(U_{i + 1, L+})

      // Left interface flux: F_{i - 1/2} = 0.5 * (F(U_{i - 1, R+}) + F(U_{i, L+})) - 0.5 * |aL_roe| * (U_{i, L+} - U_{i - 1, R+}).
      for (int j = 0; j < 2; j++) {
        aL_roe[j] = 0.5 * (f_deriv_umR_evol[j] + f_deriv_uiL_evol[j]);
      }
      for (int j = 0; j < 2; j++) {
        fluxL[j] = 0.5 * (f_umR_evol[j] + f_uiL_evol[j]) - 0.5 * fabs(aL_roe[j]) * (uiL_evol[j] - umR_evol[j]);
      }

      // Right interface flux: F_{i + 1/2} = 0.5 * (F(U_{i + 1, L+}) + F(U_{i, R+})) - 0.5 * |aR_roe| * (U_{i + 1, L+} - u_{i, R+}).
      for (int j = 0; j < 2; j++) {
        aR_roe[j] = 0.5 * (f_deriv_uiR_evol[j] + f_deriv_upL_evol[j]);
      }
      for (int j = 0; j < 2; j++) {
        fluxR[j] = 0.5 * (f_uiR_evol[j] + f_upL_evol[j]) - 0.5 * fabs(aR_roe[j]) * (upL_evol[j] - uiR_evol[j]);
      }

      // Update the conserved variable.
      for (int j = 0; j < 2; j++) {
        un[(i * 2) + j] = u[(i * 2) + j] - (dt / dx) * (fluxR[j] - fluxL[j]);
      }
    }

    // Copy un -> u (updated conserved variable vector to new conserved variable vector).
    for (int i = 0; i <= nx + 3; i++) {
      for (int j = 0; j < 2; j++) {
        u[(i * 2) + j] = un[(i * 2) + j];
      }
    }

    // Apply simple boundary conditions (transmissive).
    for (int j = 0; j < 2; j++) {
      u[(0 * 2) + j] = u[(2 * 2) + j];
      u[(1 * 2) + j] = u[(2 * 2) + j];
      u[((nx + 2) * 2) + j] = u[((nx + 1) * 2) + j];
      u[((nx + 3) * 2) + j] = u[((nx + 1) * 2) + j];
    }

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 1; i <= nx; i++) {
    double x = x0 + (i - 0.5) * dx;
    printf(\"%g %g %g\\n\", x, u[(i * 2) + 0], u[(i * 2) + 1]);
  }

  free(u);
  free(un);
  free(slope);

  free(local_alpha);
  
  free(umL);
  free(umR);
  free(uiL);
  free(uiR);
  free(upL);
  free(upR);

  free(f_umL);
  free(f_umR);
  free(f_uiL);
  free(f_uiR);
  free(f_upL);
  free(f_upR);

  free(umR_evol);
  free(uiL_evol);
  free(uiR_evol);
  free(upL_evol);

  free(f_umR_evol);
  free(f_uiL_evol);
  free(f_uiR_evol);
  free(f_upL_evol);

  free(f_deriv_umR_evol);
  free(f_deriv_uiL_evol);
  free(f_deriv_uiR_evol);
  free(f_deriv_upL_evol);

  free(fluxL);
  free(fluxR);
  
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
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           ;; Expression for flux limiter function.
           limiter-r
           ;; Left negative flux vector F(U_{i - 1, L}).
           (list-ref flux-umLs 0)
           (list-ref flux-umLs 1)
           ;; Right negative flux vector F(U_{i - 1, R}).
           (list-ref flux-umRs 0)
           (list-ref flux-umRs 1)
           ;; Left central flux vector F(U_{i, L}).
           (list-ref flux-uiLs 0)
           (list-ref flux-uiLs 1)
           ;; Right central flux vector F(U_{i, R}).
           (list-ref flux-uiRs 0)
           (list-ref flux-uiRs 1)
           ;; Left positive flux vector F(U_{i + 1, L}).
           (list-ref flux-upLs 0)
           (list-ref flux-upLs 1)
           ;; Right positive flux vector F(U_{i + 1, R}).
           (list-ref flux-upRs 0)
           (list-ref flux-upRs 1)
           ;; Evolved right negative flux vector F(U_{i - 1, R+}).
           (list-ref flux-umR-evols 0)
           (list-ref flux-umR-evols 1)
           ;; Evolved left central flux vector F(U_{i, L+}).
           (list-ref flux-uiL-evols 0)
           (list-ref flux-uiL-evols 1)
           ;; Evolved right central flux vector F(U_{i, R+}).
           (list-ref flux-uiR-evols 0)
           (list-ref flux-uiR-evols 1)
           ;; Evolved left positive flux vector F(U_{i + 1, L+}).
           (list-ref flux-upL-evols 0)
           (list-ref flux-upL-evols 1)
           ;; Eigenvalues of evolved right negative flux Jacobian F'(U_{i - 1, R+}).
           (list-ref flux-deriv-umR-evols 0)
           (list-ref flux-deriv-umR-evols 1)
           ;; Eigenvalues of evolved left central flux Jacobian F'(U_{i, L+}).
           (list-ref flux-deriv-uiL-evols 0)
           (list-ref flux-deriv-uiL-evols 1)
           ;; Eigenvalues of evolved right central flux Jacobian F'(U_{i, R+}).
           (list-ref flux-deriv-uiR-evols 0)
           (list-ref flux-deriv-uiR-evols 1)
           ;; Eigenvalues of evolved left positive flux Jacobian F'(U_{i + 1, L+}).
           (list-ref flux-deriv-upL-evols 0)
           (list-ref flux-deriv-upL-evols 1)
           ))
  code)