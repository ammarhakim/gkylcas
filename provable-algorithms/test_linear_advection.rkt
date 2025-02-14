#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

;; Define the 1D linear advection equation: du/dt + d(au)/dx = 0.
(define pde-linear-advection
  (hash
    'name "linear-advection"
    'cons-expr `u                ; conserved variable: u
    'flux-expr `(* a u)          ; flux function: f(u) = a * u
    'max-speed-expr `(abs a)     ; local wave-speed: alpha = |a|
    'parameters "a = 1.0"
    ))

;; Define simulation parameters.
(define nx 200)
(define x0 0.0)
(define x1 2.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func "(x < 1.0) ? 1.0 : 0.0")

;; Synthesize the code for a Lax-Friedrichs solver for the 1D linear advection equation.
(define code-linear-advection-lf
  (generate-lax-friedrichs-scalar-1d pde-linear-advection
                                     #:nx nx
                                     #:x0 x0
                                     #:x1 x1
                                     #:t-final t-final
                                     #:cfl cfl
                                     #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "linear_advection_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lf)))

;; Attempt to prove L-1/L-2/L-infinity stable convergence of the Lax-Friedrichs solver for the 1D linear advection equation.
(define proof-linear-advection-lf-stable
  (call-with-output-file "proof_linear_advection_lf_stable.txt"
    (lambda (out)
      (parameterize ([current-output-port out])
        (prove-lax-friedrichs-scalar-1d-stable pde-linear-advection
                                               #:nx nx
                                               #:x0 x0
                                               #:x1 x1
                                               #:t-final t-final
                                               #:cfl cfl
                                               #:init-func init-func)))
    #:exists `replace))

;; Show whether L-1/L-2/L-infinity stable convergence is satisfied.
(display "L-1/L-2/L-infinity stable convergence: ")
(display proof-linear-advection-lf-stable)
(display "\n")

;; Attempt to prove the total variation diminishing (TVD) property of the Lax-Friedrichs solver for the 1D linear advection equation.
(define proof-linear-advection-lf-tvd
  (call-with-output-file "proof_linear_advection_lf_tvd.txt"
    (lambda (out)
      (parameterize ([current-output-port out])
        (prove-lax-friedrichs-scalar-1d-tvd pde-linear-advection
                                            #:nx nx
                                            #:x0 x0
                                            #:x1 x1
                                            #:t-final t-final
                                            #:cfl cfl
                                            #:init-func init-func)))
    #:exists `replace))


;; Show whether the total variation diminishing (TVD) property is satisfied.
(display "Total variation diminishing (TVD): ")
(display proof-linear-advection-lf-tvd)
(display "\n")

;; Attempt to prove the Lax entropy property (for weak solutions) of the Lax-Friedrichs solver for the 1D linear advection equation.
(define proof-linear-advection-lf-entropy
  (call-with-output-file "proof_linear_advection_lf_entropy.txt"
    (lambda (out)
      (parameterize ([current-output-port out])
        (prove-lax-friedrichs-scalar-1d-entropy pde-linear-advection
                                                #:nx nx
                                                #:x0 x0
                                                #:x1 x1
                                                #:t-final t-final
                                                #:cfl cfl
                                                #:init-func init-func)))
    #:exists `replace))


;; Show whether the Lax entropy property (for weak solutions) is satisfied.
(display "Lax entropy property (for weak solutions): ")
(display proof-linear-advection-lf-entropy)
(display "\n")