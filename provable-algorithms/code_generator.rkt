#lang racket

(provide generate-lax-friedrichs-scalar-1d)

(define (flux-substitute flux-expr cons-expr var-name)
  (string-replace flux-expr cons-expr var-name))

;; -----------------------------------------------------------
;; Lax–Friedrichs (Finite-Difference) Solver for 1D Scalar PDE
;; -----------------------------------------------------------
(define (generate-lax-friedrichs-scalar-1d pde
                                           #:nx [nx 200]
                                           #:x0 [x0 0.0]
                                           #:x1 [x1 2.0]
                                           #:t-final [t-final 1.0]
                                           #:cfl [cfl 0.95]
                                           #:init-func
                                            [init-func "(x < 1.0) ? 1.0 : 0.0"])
 "Generate a C code string that solves the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: C code for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define cons-expr (hash-ref pde 'cons-expr))
  (define flux-expr (hash-ref pde 'flux-expr))
  (define max-speed-expr (hash-ref pde 'max-speed-expr))
  (define parameters (hash-ref pde 'parameters))

  (define flux-um (flux-substitute flux-expr cons-expr "um"))
  (define flux-ui (flux-substitute flux-expr cons-expr "ui"))
  (define flux-up (flux-substitute flux-expr cons-expr "up"))

  (define max-speed-code (flux-substitute max-speed-expr cons-expr "u[i]"))

  (define parameter-code (cond
    [(non-empty-string? parameters) (string-append "double " parameters ";")]
    [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE for PDE: ~a
// Lax–Friedrichs first-order finite-difference solver for a scalar PDE in 1D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int NX = ~a;
  const double X0 = ~a;
  const double X1 = ~a;
  const double L = (X1 - X0);
  const double dx = L / NX;

  // Time-stepper setup.
  const double CFL = ~a;
  const double TFINAL = ~a;

  // Arrays for storing solution.
  double *u  = (double*) malloc((NX + 2) * sizeof(double));
  double *un = (double*) malloc((NX + 2) * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= NX+1; i++) {
    double x = X0 + (i - 0.5) * dx;
    u[i] = ~a;  // init-func in C.
  }

  double t = 0.0;
  while (t < TFINAL) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    for(int i = 1; i <= NX; i++) {
      double uVal = u[i];
      double localAlpha = ~a; // max-speed-expr in C.
      
      if(localAlpha > alpha) {
        alpha = localAlpha;
      }
    }

    // Compute stable time step from alpha.
    double dt = CFL * dx / alpha;

    // If stepping beyond TFINAL, adjust dt accordingly.
    if (t + dt > TFINAL) {
      dt = TFINAL - t;
    }

    // Compute fluxes with Lax-Friedrichs approximation and update conserved variable.
    for (int i = 1; i <= NX; i++) {
      double um = u[i - 1];
      double ui = u[i];
      double up = u[i + 1];

      // Evaluate flux for each conserved variable.
      double f_um = ~a; // f(u_{i - 1}).
      double f_ui = ~a; // f(u_i).
      double f_up = ~a; // f(u_{i + 1}).

      // Left interface flux: F_{i - 1/2}.
      double fluxL = 0.5 * (f_um + f_ui) - 0.5 * alpha * (ui - um);
      // fluxL = 0.5 * (f(u_{i - 1}) + f(u_i)) - 0.5 * alpha * (u_i - u_{i - 1}).

      // Right interface flux: F_{i + 1/2}.
      double fluxR = 0.5 * (f_ui + f_up) - 0.5 * alpha * (up - ui);
      // fluxR = 0.5 * (f(u_{u + 1}) + f(u_i)) - 0.5 * alpha * (u_{i + 1} - u_i).

      // Update the conserved variable vector.
      un[i] = ui - (dt / dx) * (fluxR - fluxL);
    }

    // Copy un -> u (updated conserved variables to new conserved variables).
    for (int i = 1; i <= NX; i++) {
      u[i] = un[i];
    }

    // Apply simple boundary conditions (transmissive).
    u[0] = u[1];
    u[NX + 1] = u[NX];

    // Increment time.
    t += dt;
  }

  // Output solution to stdout.
  for (int i = 0; i <= NX + 1; i++) {
    double x = X0 + (i - 0.5) * dx;
    printf(\"%g  %g\\n\", x, u[i]);
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
           ;; Initial condition expression (e.g. (x < 1.0) ? 1.0 : 0.0).
           init-func
           ;; Expression for local wave-speed estimate.
           max-speed-code
           ;; Left flux f(u_{i - 1}).
           flux-um
           ;; Middle flux f(u_i).
           flux-ui
           ;; Right flux f(u_{i + 1}).
           flux-up
           ))
  code)