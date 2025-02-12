#lang racket/base

(require "code_generator.rkt")
(provide (all-from-out "code_generator.rkt"))

(define PDE-linear-advection
  (hash
    'name "linear-advection"
    'flux-expr "a * u"          ; flux function: f(u) = a * u
    'max-speed-expr "fabs(a)"     ; local wave-speed: alpha = |a|
    'parameters "double a = 1.0;"
    ))

(define code-linear-advection-lf
  (generate-lax-friedrichs PDE-linear-advection
                           #:nx 200
                           #:x0 0.0
                           #:x1 2.0
                           #:tfinal 0.5
                           #:cfl 0.95
                           #:init-func "(x < 1.0) ? 1.0 : 0.0"))

(with-output-to-file "linear_advection_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lf)))