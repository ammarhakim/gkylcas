#lang racket

(require "code_generator_core_training.rkt")
(require "code_generator_core_validation.rkt")
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