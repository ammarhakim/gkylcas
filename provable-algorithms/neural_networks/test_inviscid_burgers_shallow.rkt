#lang racket

(require "code_generator_core_training.rkt")
(require "code_generator_core_validation.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator_core_training.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
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
(define x0 -3.0)
(define x1 3.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func `(cond
                     [(< (abs x) 1.0) 3.0]
                     [else -1.0]))

;; Define (shallow) neural network hyperparameters.
(define neural-net-shallow
  (hash
   'max-trains 10000    ; maximum number of training steps: 10000
   'width 64            ; number of neurons in each layer: 64
   'depth 6             ; total number of layers: 6
   ))
   

;; Synthesize the code to train a Lax-Friedrichs surrogate solver for the 1D inviscid Burgers' equation using a shallow neural network.
(define code-inviscid-burgers-lax-train
  (train-lax-friedrichs-scalar-1d pde-inviscid-burgers neural-net-shallow
                                  #:nx nx
                                  #:x0 x0
                                  #:x1 x1
                                  #:t-final t-final
                                  #:cfl cfl
                                  #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_lax_train.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lax-train)))

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

;; Synthesize the code to train a Lax-Friedrichs surrogate solver for the 1D inviscid Burgers' equation (with a second-order flux extrapolation using the minmod flux limiter)
;; using a shallow neural network.
(define code-inviscid-burgers-lax-minmod-train
  (train-lax-friedrichs-scalar-1d-second-order pde-inviscid-burgers limiter-minmod neural-net-shallow
                                               #:nx nx
                                               #:x0 x0
                                               #:x1 x1
                                               #:t-final t-final
                                               #:cfl cfl
                                               #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_lax_minmod_train.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lax-minmod-train)))

;; Attempt to prove error bounds on smooth solutions obtained from surrogate solvers for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-smooth
  (call-with-output-file "proofs/proof_inviscid_burgers_smooth.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-scalar-1d-smooth pde-inviscid-burgers neural-net-shallow
                                #:nx nx
                                #:x0 x0
                                #:x1 x1
                                #:t-final t-final
                                #:cfl cfl
                                #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_inviscid_burgers_smooth.rkt")

;; Show the error bound (if applicable) on smooth solutions.
(display "Error bound (smooth solutions): ")
(display proof-inviscid-burgers-smooth)
(display "\n")

;; Attempt to prove error bounds on non-smooth solutions obtained from surrogate solvers for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-non-smooth
  (call-with-output-file "proofs/proof_inviscid_burgers_non_smooth.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-scalar-1d-non-smooth pde-inviscid-burgers neural-net-shallow
                                    #:nx nx
                                    #:x0 x0
                                    #:x1 x1
                                    #:t-final t-final
                                    #:cfl cfl
                                    #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_inviscid_burgers_non_smooth.rkt")

;; Show the error bound (if applicable) on non-smooth solutions.
(display "Error bound (non-smooth solutions): ")
(display proof-inviscid-burgers-non-smooth)
(display "\n")

;; Synthesize the code to validate any first-order surrogate solver for the 1D inviscid Burgers' equation using a shallow neural network.
(define code-inviscid-burgers-validate
  (validate-scalar-1d pde-inviscid-burgers neural-net-shallow
                      #:nx nx
                      #:x0 x0
                      #:x1 x1
                      #:t-final t-final
                      #:cfl cfl
                      #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_validate.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-validate)))

;; Synthesize the code to validate any first-order surrogate solver for the 1D inviscid Burgers' equation (with a second-order flux extrapolation using the minmod flux limiter)
;; using a shallow neural network.
(define code-inviscid-burgers-minmod-validate
  (validate-scalar-1d-second-order pde-inviscid-burgers limiter-minmod neural-net-shallow
                                   #:nx nx
                                   #:x0 x0
                                   #:x1 x1
                                   #:t-final t-final
                                   #:cfl cfl
                                   #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_minmod_validate.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-minmod-validate)))