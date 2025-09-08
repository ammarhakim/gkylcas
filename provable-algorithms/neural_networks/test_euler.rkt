#lang racket

(require "code_generator_core_training.rkt")
(require "code_generator_validation.rkt")
(require "code_generator_matrix_training.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator_core_training.rkt"))
(provide (all-from-out "code_generator_matrix_training.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1D Euler equations (density, x-momentum, and total energy components).
(define pde-system-euler
  (hash
   'name "euler"
   'cons-exprs (list
                `rho
                `mom_x
                `energy)               ; conserved variables: density, x-momentum, total energy
   'flux-exprs (list
                `mom_x
                `(+ (/ (* mom_x mom_x) rho) (* (- gamma 1.0) (- energy (* 0.5 (/ (* mom_x mom_x) rho)))))
                `(* (+ energy (* (- gamma 1.0) (- energy (* 0.5 (/ (* mom_x mom_x) rho))))) (/ mom_x rho)))
                                       ; flux vector
   'max-speed-exprs (list
                     `(abs (- (/ mom_x rho) (sqrt (/ (* gamma (* (- gamma 1.0) (- energy (* 0.5 (/ (* mom_x mom_x) rho))))) rho))))
                     `(abs (/ mom_x rho))
                     `(abs (+ (/ mom_x rho) (sqrt (/ (* gamma (* (- gamma 1.0) (- energy (* 0.5 (/ (* mom_x mom_x) rho))))) rho)))))
                                       ; local wave-speeds
   'parameters (list
                `(define gamma 1.4))   ; adiabatic index: gamma = 1.4
   ))

;; Define simulation parameters.
(define nx 800)
(define x0 0.0)
(define x1 1.0)
(define t-final 0.1)
(define cfl 0.95)
(define init-funcs (list
                    `(cond
                       [(< x 0.5) 3.0]
                       [else 1.0])
                    `(cond
                       [(< x 0.5) 0.0]
                       [else 0.0])
                    `(cond
                       [(< x 0.5) 7.5]
                       [else 2.5])))

;; Define (shallow) neural network hyperparameters.
(define neural-net-shallow
  (hash
   'max-trains 10000    ; maximum number of training steps: 10000
   'width 64            ; number of neurons in each layer: 64
   'depth 6             ; total number of layers: 6
   ))

;; Synthesize the code to train a Lax-Friedrichs surrogate solver for the 1D Euler equations using a shallow neural network.
(define code-euler-lax-train
  (train-lax-friedrichs-vector3-1d pde-system-euler neural-net-shallow
                                   #:nx nx
                                   #:x0 x0
                                   #:x1 x1
                                   #:t-final t-final
                                   #:cfl cfl
                                   #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/euler_lax_train.c"
  #:exists 'replace
  (lambda ()
    (display code-euler-lax-train)))

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

;; Synthesize the code to train a Lax-Friedrichs surrogate solver for the 1D Euler equations (with a second-order flux extrapolation using the minmod flux limiter)
;; using a shallow neural network.
(define code-euler-lax-minmod-train
  (train-lax-friedrichs-vector3-1d-second-order pde-system-euler limiter-minmod neural-net-shallow
                                                #:nx nx
                                                #:x0 x0
                                                #:x1 x1
                                                #:t-final t-final
                                                #:cfl cfl
                                                #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/euler_lax_minmod_train.c"
  #:exists 'replace
  (lambda ()
    (display code-euler-lax-minmod-train)))

;; Synthesize the code to validate any first-order surrogate solver for the 1D Euler equations using a shallow neural network.
(define code-euler-validate
  (validate-vector3-1d pde-system-euler neural-net-shallow
                       #:nx nx
                       #:x0 x0
                       #:x1 x1
                       #:t-final t-final
                       #:cfl cfl
                       #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/euler_validate.c"
  #:exists 'replace
  (lambda ()
    (display code-euler-validate)))