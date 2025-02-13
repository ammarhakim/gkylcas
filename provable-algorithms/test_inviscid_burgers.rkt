#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

(define pde-inviscid-burgers
  (hash
    'name "inviscid-burgers"
    'cons-expr "u"             ; conserved variable: u
    'flux-expr "0.5 * u * u"   ; flux function: f(u) = 0.5 * u^2
    'max-speed-expr "fabs(u)"  ; local wave-speed: alpha = |u|
    'parameters ""
    ))

(define nx 200)
(define x0 -1.0)
(define x1 1.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func "(x < 0.0) ? 1.0 : 0.0")

(define code-inviscid-burgers-lf
  (generate-lax-friedrichs-scalar-1d pde-inviscid-burgers
                                     #:nx nx
                                     #:x0 x0
                                     #:x1 x1
                                     #:t-final t-final
                                     #:cfl cfl
                                     #:init-func init-func))

(define proof-inviscid-burgers-lf-stability
  (prove-lax-friedrichs-scalar-1d-stability pde-inviscid-burgers
                                            #:nx nx
                                            #:x0 x0
                                            #:x1 x1
                                            #:t-final t-final
                                            #:cfl cfl
                                            #:init-func init-func))

(with-output-to-file "inviscid_burgers_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lf)))

(display proof-inviscid-burgers-lf-stability)