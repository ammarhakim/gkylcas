#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

(define pde-linear-advection
  (hash
    'name "linear-advection"
    'cons-expr "u"                ; conserved variable: u
    'flux-expr "a * u"            ; flux function: f(u) = a * u
    'max-speed-expr "fabs(a)"     ; local wave-speed: alpha = |a|
    'parameters "a = 1.0"
    ))

(define nx 200)
(define x0 0.0)
(define x1 2.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func "(x < 1.0) ? 1.0 : 0.0")

(define code-linear-advection-lf
  (generate-lax-friedrichs-scalar-1d pde-linear-advection
                                     #:nx nx
                                     #:x0 x0
                                     #:x1 x1
                                     #:t-final t-final
                                     #:cfl cfl
                                     #:init-func init-func))

(define proof-linear-advection-lf-stability
  (prove-lax-friedrichs-scalar-1d-stability pde-linear-advection
                                            #:nx nx
                                            #:x0 x0
                                            #:x1 x1
                                            #:t-final t-final
                                            #:cfl cfl
                                            #:init-func init-func))

(with-output-to-file "linear_advection_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lf)))

(display proof-linear-advection-lf-stability)