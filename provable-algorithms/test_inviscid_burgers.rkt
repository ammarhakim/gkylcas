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
    'parameters ""
    ))

;; Define simulation parameters.
(define nx 200)
(define x0 -1.0)
(define x1 1.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func "(x < 0.0) ? 1.0 : 0.0")

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

;; Attempt to prove L-1/L-2/L-infinity stable convergence of the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lf-stable
  (call-with-output-file "proof_inviscid_burgers_lf_stable.txt"
    (lambda (out)
      (parameterize ([current-output-port out])
        (prove-lax-friedrichs-scalar-1d-stable pde-inviscid-burgers
                                               #:nx nx
                                               #:x0 x0
                                               #:x1 x1
                                               #:t-final t-final
                                               #:cfl cfl
                                               #:init-func init-func)))
    #:exists `replace))

;; Show whether L-1/L-2/L-infinity stable convergence is satisfied.
(display "L-1/L-2/L-infinity stable convergence: ")
(display proof-inviscid-burgers-lf-stable)
(display "\n")

;; Attempt to prove the total variation diminishing (TVD) property of the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lf-tvd
  (call-with-output-file "proof_inviscid_burgers_lf_tvd.txt"
    (lambda (out)
      (parameterize ([current-output-port out])
        (prove-lax-friedrichs-scalar-1d-tvd pde-inviscid-burgers
                                            #:nx nx
                                            #:x0 x0
                                            #:x1 x1
                                            #:t-final t-final
                                            #:cfl cfl
                                            #:init-func init-func)))
    #:exists `replace))


;; Show whether the total variation diminishing (TVD) property is satisfied.
(display "Total variation diminishing (TVD): ")
(display proof-inviscid-burgers-lf-tvd)
(display "\n")

;; Attempt to prove the Lax entropy property (for weak solutions) of the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lf-entropy
  (call-with-output-file "proof_inviscid_burgers_lf_entropy.txt"
    (lambda (out)
      (parameterize ([current-output-port out])
        (prove-lax-friedrichs-scalar-1d-entropy pde-inviscid-burgers
                                                #:nx nx
                                                #:x0 x0
                                                #:x1 x1
                                                #:t-final t-final
                                                #:cfl cfl
                                                #:init-func init-func)))
    #:exists `replace))


;; Show whether the Lax entropy property (for weak solutions) is satisfied.
(display "Lax entropy property (for weak solutions): ")
(display proof-inviscid-burgers-lf-entropy)
(display "\n")