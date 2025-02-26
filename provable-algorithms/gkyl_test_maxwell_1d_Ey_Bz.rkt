#lang racket

(require "gkyl_code_generator_lax_vector.rkt")
(require "prover_core.rkt")
(require "prover_vector.rkt")
(provide (all-from-out "gkyl_code_generator_lax_vector.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "gkyl_code")) (make-directory "gkyl_code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1D Maxwell equations (Ey and Bz components).
(define pde-system-maxwell-1d-Ey-Bz
  (hash
    'name "maxwell_EyBz"
    'cons-exprs (list
                 `Ey
                 `Bz)                   ; conserved variables: electric field (y-component), magnetic field (z-component)
    'flux-exprs (list
                 `(* (* c c) Bz)
                 `Ey)                   ; flux vector
    'max-speed-exprs (list
                      `(abs c)
                      `(abs c))         ; local wave-speeds
    'parameters (list
                 `(define c 1.0)        ; speed of light: c = 1.0
                 `(define e_fact 1.0)   ; electric field divergence error propagation: e_fact = 1.0
                 `(define b_fact 1.0))  ; magnetic field divergence error propagation: b_fact = 1.0
    ))

;; Define simulation parameters.
(define nx 200)
(define x0 -1.5)
(define x1 1.5)
(define t-final 1.0)
(define cfl 0.95)
(define init-funcs (list
                    0.0
                    `(cond
                       [(< x 0.0) 0.5]
                       [else -0.5])))

;; Synthesize the Gkeyll header code for a Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components).
(define code-maxwell-1d-Ey-Bz-lax-header
  (gkyl-generate-lax-friedrichs-vector2-1d-header pde-system-maxwell-1d-Ey-Bz
                                                  #:nx nx
                                                  #:x0 x0
                                                  #:x1 x1
                                                  #:t-final t-final
                                                  #:cfl cfl
                                                  #:init-funcs init-funcs))

;; Output the header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_maxwell_EyBz_lax.h"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-Ey-Bz-lax-header)))

;; Synthesize the Gkeyll private header code for a Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components).
(define code-maxwell-1d-Ey-Bz-lax-priv-header
  (gkyl-generate-lax-friedrichs-vector2-1d-priv-header pde-system-maxwell-1d-Ey-Bz
                                                       #:nx nx
                                                       #:x0 x0
                                                       #:x1 x1
                                                       #:t-final t-final
                                                       #:cfl cfl
                                                       #:init-funcs init-funcs))

;; Output the private header code to a file.
(with-output-to-file "gkyl_code/gkyl_wv_maxwell_EyBz_lax_priv.h"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-Ey-Bz-lax-priv-header)))

;; Synthesize the Gkeyll source code for a Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components).
(define code-maxwell-1d-Ey-Bz-lax-source
  (gkyl-generate-lax-friedrichs-vector2-1d-source pde-system-maxwell-1d-Ey-Bz
                                                  #:nx nx
                                                  #:x0 x0
                                                  #:x1 x1
                                                  #:t-final t-final
                                                  #:cfl cfl
                                                  #:init-funcs init-funcs))

;; Output the source code to a file.
(with-output-to-file "gkyl_code/wv_maxwell_EyBz_lax.c"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-Ey-Bz-lax-source)))