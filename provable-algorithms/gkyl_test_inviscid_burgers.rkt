#lang racket

(require "gkyl_code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "gkyl_code_generator.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "gkyl_code")) (make-directory "gkyl_code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1D inviscid Burgers' equation: du/dt + u du/dx = 0.
(define pde-inviscid-burgers
  (hash
   'name "burgers"
   'cons-expr `u             ; conserved variable: u
   'flux-expr `(* 0.5 u u)   ; flux function: f(u) = 0.5 * u^2
   'max-speed-expr `(abs u)  ; local wave-speed: alpha = |u|
   'parameters `()
   ))

;; Define simulation parameters.
(define nx 200)
(define x0 -1.0)
(define x1 1.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func `(cond
                     [(< x 0.0) 1.0]
                     [else 0.0]))

;; Synthesize the Gkeyll header code for a Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-lax-header
  (gkyl-generate-lax-friedrichs-scalar-1d-header pde-inviscid-burgers
                                                 #:nx nx
                                                 #:x0 x0
                                                 #:x1 x1
                                                 #:t-final t-final
                                                 #:cfl cfl
                                                 #:init-func init-func))

;; Output the header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_burgers_lax.h"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lax-header)))

;; Synthesize the Gkeyll private header code for a Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-lax-priv-header
  (gkyl-generate-lax-friedrichs-scalar-1d-priv-header pde-inviscid-burgers
                                                      #:nx nx
                                                      #:x0 x0
                                                      #:x1 x1
                                                      #:t-final t-final
                                                      #:cfl cfl
                                                      #:init-func init-func))

;; Output the private header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_burgers_lax_priv.h"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lax-priv-header)))

;; Synthesize the Gkeyll source code for a Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-lax-source
  (gkyl-generate-lax-friedrichs-scalar-1d-source pde-inviscid-burgers
                                                 #:nx nx
                                                 #:x0 x0
                                                 #:x1 x1
                                                 #:t-final t-final
                                                 #:cfl cfl
                                                 #:init-func init-func))

;; Output the source code to a file.
(with-output-to-file "gkyl_code/wv_burgers_lax.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lax-source)))

;; Synthesize the Gkeyll header code for a Roe solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-roe-header
  (gkyl-generate-roe-scalar-1d-header pde-inviscid-burgers
                                      #:nx nx
                                      #:x0 x0
                                      #:x1 x1
                                      #:t-final t-final
                                      #:cfl cfl
                                      #:init-func init-func))

;; Output the header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_burgers_roe.h"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-roe-header)))

;; Synthesize the Gkeyll private header code for a Roe solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-roe-priv-header
  (gkyl-generate-roe-scalar-1d-priv-header pde-inviscid-burgers
                                           #:nx nx
                                           #:x0 x0
                                           #:x1 x1
                                           #:t-final t-final
                                           #:cfl cfl
                                           #:init-func init-func))

;; Output the private header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_burgers_roe_priv.h"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-roe-priv-header)))

;; Synthesize the Gkeyll source code for a Roe solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-roe-source
  (gkyl-generate-roe-scalar-1d-source pde-inviscid-burgers
                                      #:nx nx
                                      #:x0 x0
                                      #:x1 x1
                                      #:t-final t-final
                                      #:cfl cfl
                                      #:init-func init-func))

;; Output the source code to a file.
(with-output-to-file "gkyl_code/wv_burgers_roe.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-roe-source)))