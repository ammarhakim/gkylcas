#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

;; Define the 1D Maxwell equations.
(define pde-system-maxwell-1d
  (hash
    'name "maxwell-1d"
    'cons-exprs (list
                 `Ey
                 `Bz)               ; conserved variables: electric field (y-component), magnetic field (z-component)
    'flux-exprs (list
                 `(* (* c c) Bz)
                 `Ey)               ; flux vector
    'max-speed-exprs (list
                      `(abs c)
                      `(abs c))     ; local wave-speeds
    'parameters "c = 1.0"
    ))

;; Define simulation parameters.
(define nx 200)
(define x0 -1.5)
(define x1 1.5)
(define t-final 1.0)
(define cfl 0.95)
(define init-funcs (list
                    "0.0"
                    "(x < 0.0) ? 0.5 : -0.5"))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D Maxwell equations.
(define code-maxwell-1d-lf
  (generate-lax-friedrichs-vector2-1d pde-system-maxwell-1d
                                      #:nx nx
                                      #:x0 x0
                                      #:x1 x1
                                      #:t-final t-final
                                      #:cfl cfl
                                      #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "maxwell_1d_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-lf)))

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1D Maxwell equations.
(define proof-maxwell-1d-lf-cfl-stability
  (call-with-output-file "proof_maxwell_1d_lf_cfl_stability.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-vector2-1d-cfl-stability pde-system-maxwell-1d
                                                       #:nx nx
                                                       #:x0 x0
                                                       #:x1 x1
                                                       #:t-final t-final
                                                       #:cfl cfl
                                                       #:init-funcs init-funcs)))
    #:exists `replace))

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-maxwell-1d-lf-cfl-stability)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D Maxwell equations.
(define proof-maxwell-1d-lf-local-lipschitz
  (call-with-output-file "proof_maxwell_1d_lf_local_lipschitz.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-vector2-1d-local-lipschitz pde-system-maxwell-1d
                                                         #:nx nx
                                                         #:x0 x0
                                                         #:x1 x1
                                                         #:t-final t-final
                                                         #:cfl cfl
                                                         #:init-funcs init-funcs)))
    #:exists `replace))

;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-maxwell-1d-lf-local-lipschitz)
(display "\n")