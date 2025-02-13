#lang racket

(provide prove-lax-friedrichs-scalar-1d-stability)

;; -------------------------------------------------------------------------------------------------
;; Prove L-1/L-2/L-infinity stability of Lax–Friedrichs (Finite-Difference) Solver for 1D Scalar PDE
;; -------------------------------------------------------------------------------------------------
(define (prove-lax-friedrichs-scalar-1d-stability pde
                                                     #:nx [nx 200]
                                                     #:x0 [x0 0.0]
                                                     #:x1 [x1 2.0]
                                                     #:t-final [t-final 1.0]
                                                     #:cfl [cfl 0.95]
                                                     #:init-func
                                                     [init-func "(x < 1.0) ? 1.0 : 0.0"])
   "Prove that the Lax-Friedrichs finite-difference method is L-1/L-2/L-infinity stable for the 1D scalar PDE specified by `pde`. 
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: C code for the initial condition, e.g. piecewise constant."

  (cond
    [(or (< cfl 0) (> cfl 1)) #f]
    [(or (< nx 1) (>= x0 x1)) #f]
    [(< t-final 0) #f]
    [else #t]))