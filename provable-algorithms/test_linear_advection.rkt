#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

;; Define the 1D linear advection equation: du/dt + d(au)/dx = 0.
(define pde-linear-advection
  (hash
    'name "linear-advection"
    'cons-expr `u                ; conserved variable: u
    'flux-expr `(* a u)          ; flux function: f(u) = a * u
    'max-speed-expr `(abs a)     ; local wave-speed: alpha = |a|
    'parameters "a = 1.0"
    ))

;; Define simulation parameters.
(define nx 200)
(define x0 0.0)
(define x1 2.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func "(x < 1.0) ? 1.0 : 0.0")

;; Synthesize the code for a Lax-Friedrichs solver for the 1D linear advection equation.
(define code-linear-advection-lf
  (generate-lax-friedrichs-scalar-1d pde-linear-advection
                                     #:nx nx
                                     #:x0 x0
                                     #:x1 x1
                                     #:t-final t-final
                                     #:cfl cfl
                                     #:init-func init-func))

;; Attempt to prove L-1/L-2/L-infinity stability of the Lax-Friedrichs solver for the 1D linear advection equation.
(define proof-linear-advection-lf-stability
  (prove-lax-friedrichs-scalar-1d-stability pde-linear-advection
                                            #:nx nx
                                            #:x0 x0
                                            #:x1 x1
                                            #:t-final t-final
                                            #:cfl cfl
                                            #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "linear_advection_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lf)))

;; Show whether L-1/L-2/L-infinity stability is satisfied.
(display proof-linear-advection-lf-stability)