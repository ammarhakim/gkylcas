#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

;; Define the 1D inviscid Burgers' equation: du/dt + u du/dx = 0.
(define pde-inviscid-burgers
  (hash
    'name "inviscid-burgers"
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

;; Synthesize the code for a Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-lf
  (generate-lax-friedrichs-scalar-1d pde-inviscid-burgers
                                     #:nx nx
                                     #:x0 x0
                                     #:x1 x1
                                     #:t-final t-final
                                     #:cfl cfl
                                     #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "inviscid_burgers_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lf)))

;; Attempt to prove hyperbolicity of the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lf-hyperbolicity
  (call-with-output-file "proof_inviscid_burgers_lf_hyperbolicity.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-scalar-1d-hyperbolicity pde-inviscid-burgers
                                                      #:nx nx
                                                      #:x0 x0
                                                      #:x1 x1
                                                      #:t-final t-final
                                                      #:cfl cfl
                                                      #:init-func init-func)))
    #:exists `replace))

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity: ")
(display proof-inviscid-burgers-lf-hyperbolicity)
(display "\n")

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lf-cfl-stability
  (call-with-output-file "proof_inviscid_burgers_lf_cfl_stability.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-scalar-1d-cfl-stability pde-inviscid-burgers
                                                      #:nx nx
                                                      #:x0 x0
                                                      #:x1 x1
                                                      #:t-final t-final
                                                      #:cfl cfl
                                                      #:init-func init-func)))
    #:exists `replace))

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-inviscid-burgers-lf-cfl-stability)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lf-local-lipschitz
  (call-with-output-file "proof_inviscid_burgers_lf_local_lipschitz.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-scalar-1d-local-lipschitz pde-inviscid-burgers
                                                        #:nx nx
                                                        #:x0 x0
                                                        #:x1 x1
                                                        #:t-final t-final
                                                        #:cfl cfl
                                                        #:init-func init-func)))
    #:exists `replace))


;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-inviscid-burgers-lf-local-lipschitz)
(display "\n")