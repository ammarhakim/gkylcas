#lang racket

(require "gkyl_code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "gkyl_code_generator.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "gkyl_code")) (make-directory "gkyl_code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1D linear advection equation: du/dt + d(au)/dx = 0.
(define pde-linear-advection
  (hash
   'name "advect"
   'cons-expr `u                ; conserved variable: u
   'flux-expr `(* a u)          ; flux function: f(u) = a * u
   'max-speed-expr `(abs a)     ; local wave-speed: alpha = |a|
   'parameters `(define a 1.0)  ; advection speed: a = 1.0
   ))

;; Define simulation parameters.
(define nx 200)
(define x0 0.0)
(define x1 2.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func `(cond
                     [(< x 1.0) 1.0]
                     [else 0.0]))

;; Synthesize the Gkeyll header code for a Lax-Friedrichs solver for the 1D linear advection equation.
(define code-linear-advection-lax-header
  (gkyl-generate-lax-friedrichs-scalar-1d-header pde-linear-advection
                                                 #:nx nx
                                                 #:x0 x0
                                                 #:x1 x1
                                                 #:t-final t-final
                                                 #:cfl cfl
                                                 #:init-func init-func))

;; Output the header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_advect.h"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lax-header)))

;; Synthesize the Gkeyll private header code for a Lax-Friedrichs solver for the 1D linear advection equation.
(define code-linear-advection-lax-priv-header
  (gkyl-generate-lax-friedrichs-scalar-1d-priv-header pde-linear-advection
                                                      #:nx nx
                                                      #:x0 x0
                                                      #:x1 x1
                                                      #:t-final t-final
                                                      #:cfl cfl
                                                      #:init-func init-func))

;; Output the private header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_advect_priv.h"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lax-priv-header)))

;; Synthesize the Gkeyll source code for a Lax-Friedrichs solver for the 1D linear advection equation.
(define code-linear-advection-lax-source
  (gkyl-generate-lax-friedrichs-scalar-1d-source pde-linear-advection
                                                 #:nx nx
                                                 #:x0 x0
                                                 #:x1 x1
                                                 #:t-final t-final
                                                 #:cfl cfl
                                                 #:init-func init-func))

;; Output the source code to a file.
(with-output-to-file "gkyl_code/wv_advect.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lax-source)))