#lang racket

(require "code_generator_core_training.rkt")
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
   'width 3             ; number of neurons in each layer: 3
   'depth 1             ; total number of layers: 1
   ))
   

;; Synthesize the code to train a Lax-Friedrichs solver for the 1D linear advection equation using a shallow neural network.
(define code-linear-advection-lax
  (train-lax-friedrichs-scalar-1d pde-linear-advection neural-net-shallow
                                  #:nx nx
                                  #:x0 x0
                                  #:x1 x1
                                  #:t-final t-final
                                  #:cfl cfl
                                  #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/linear_advection_lax.c"
  #:exists 'replace
  (lambda ()
    (display code-linear-advection-lax)))