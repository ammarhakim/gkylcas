#lang racket/base

(require "code_generator.rkt")
(provide (all-from-out "code_generator.rkt"))

(define PDE-inviscid-burgers
  (hash
    'name "inviscid-burgers"
    'flux-expr "0.5 * u * u"   ; flux function: f(u) = 0.5 * u^2
    'max-speed-expr "fabs(u)"  ; local wave-speed: alpha = |u|
    'parameters ""
    ))

(define code-inviscid-burgers-lf
  (generate-lax-friedrichs PDE-inviscid-burgers
                           #:nx 200
                           #:x0 0.0
                           #:x1 2.0
                           #:tfinal 0.5
                           #:cfl 0.95
                           #:init-func "(x < 0.0) ? 1.0 : 0.0"))

(with-output-to-file "inviscid_burgers_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lf)))