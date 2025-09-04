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

;; Define the 1D linear advection equation: du/dt + d(au)/dx = 0.
(define pde-linear-advection
  (hash
   'name "linear-advection"
   'cons-expr `u                 ; conserved variable: u
   'flux-expr `(* a u)           ; flux function: f(u) = a * u
   'max-speed-expr `(abs a)      ; local wave-speed: alpha = |a|
   'parameters (list
                `(define a 1.0)) ; advection speed: a = 1.0
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

;; Define (shallow) neural network hyperparameters.
(define neural-net-shallow
  (hash
   'max-trains 10000    ; maximum number of training steps: 10000
   'width 64            ; number of neurons in each layer: 64
   'depth 4             ; total number of layers: 4
   ))
   

;; Synthesize the code to train a Lax-Friedrichs surrogate solver for the 1D linear advection equation using a shallow neural network.
(define code-linear-advection-lax-train
  (train-lax-friedrichs-scalar-1d pde-linear-advection neural-net-shallow
                                  #:nx nx
                                  #:x0 x0
                                  #:x1 x1
                                  #:t-final t-final
                                  #:cfl cfl
                                  #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/linear_advection_lax_train.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lax-train)))

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

;; Synthesize the code to train a Lax-Friedrichs surrogate solver for the 1D linear advection equation (with a second-order flux extrapolation using the minmod flux limiter)
;; using a shallow neural network.
(define code-linear-advection-lax-minmod-train
  (train-lax-friedrichs-scalar-1d-second-order pde-linear-advection limiter-minmod neural-net-shallow
                                               #:nx nx
                                               #:x0 x0
                                               #:x1 x1
                                               #:t-final t-final
                                               #:cfl cfl
                                               #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/linear_advection_lax_minmod_train.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lax-minmod-train)))

;; Attempt to prove error bounds on smooth solutions obtained from surrogate solvers for the 1D linear advection equation.
(define proof-linear-advection-smooth
  (call-with-output-file "proofs/proof_linear_advection_smooth.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n\n")
        (prove-scalar-1d-smooth pde-linear-advection neural-net-shallow
                                #:nx nx
                                #:x0 x0
                                #:x1 x1
                                #:t-final t-final
                                #:cfl cfl
                                #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_linear_advection_smooth.rkt")

;; Show the error bound (if applicable) on smooth solutions.
(display "Error bound (smooth solutions): ")
(display proof-linear-advection-smooth)
(display "\n")

;; Synthesize the code to validate any first-order surrogate solver for the 1D linear advection equation using a shallow neural network.
(define code-linear-advection-validate
  (validate-scalar-1d pde-linear-advection neural-net-shallow
                      #:nx nx
                      #:x0 x0
                      #:x1 x1
                      #:t-final t-final
                      #:cfl cfl
                      #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/linear_advection_validate.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-validate)))

;; Synthesize the code to validate any first-order surrogate solver for the 1D linear advection equation (with a second-order flux extrapolation using the minmod flux limiter)
;; using a shallow neural network.
(define code-linear-advection-minmod-validate
  (validate-scalar-1d-second-order pde-linear-advection limiter-minmod neural-net-shallow
                                   #:nx nx
                                   #:x0 x0
                                   #:x1 x1
                                   #:t-final t-final
                                   #:cfl cfl
                                   #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/linear_advection_minmod_validate.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-minmod-validate)))