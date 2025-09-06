#lang racket

(require "code_generator_core_training.rkt")
(require "code_generator_validation.rkt")
(require "code_generator_vector_training.rkt")
(provide (all-from-out "code_generator_core_training.rkt"))
(provide (all-from-out "code_generator_vector_training.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1D isothermal Euler equations (density and x-momentum components).
(define pde-system-isothermal-euler
  (hash
   'name "isothermal-euler"
   'cons-exprs (list
                `rho
                `mom_x)                                      ; conserved variables: density, x-momentum
   'flux-exprs (list
                `mom_x
                `(+ (/ (* mom_x mom_x) rho) (* rho vt vt)))  ; flux vector
   'max-speed-exprs (list
                     `(abs (- (/ mom_x rho) vt))
                     `(abs (+ (/ mom_x rho) vt)))            ; local wave-speeds
   'parameters (list
                `(define vt 1.0))                            ; thermal velocity: vt = 1.0
   ))

;; Define simulation parameters.
(define nx 200)
(define x0 0.0)
(define x1 1.0)
(define t-final 0.1)
(define cfl 0.95)
(define init-funcs (list
                    `(cond
                       [(< x 0.5) 3.0]
                       [else 1.0])
                    `(cond
                       [(< x 0.5) 1.5]
                       [else 0.0])))

;; Define (shallow) neural network hyperparameters.
(define neural-net-shallow
  (hash
   'max-trains 10000    ; maximum number of training steps: 10000
   'width 64            ; number of neurons in each layer: 64
   'depth 6             ; total number of layers: 6
   ))

;; Synthesize the code to train a Lax-Friedrichs surrogate solver for the 1D isothermal Euler equations using a shallow neural network.
(define code-isothermal-euler-lax-train
  (train-lax-friedrichs-vector2-1d pde-system-isothermal-euler neural-net-shallow
                                   #:nx nx
                                   #:x0 x0
                                   #:x1 x1
                                   #:t-final t-final
                                   #:cfl cfl
                                   #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/isothermal_euler_lax_train.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-lax-train)))

;; Synthesize the code to validate any first-order surrogate solver for the 1D isothermal Euler equations using a shallow neural network.
(define code-isothermal-euler-validate
  (validate-vector2-1d pde-system-isothermal-euler neural-net-shallow
                       #:nx nx
                       #:x0 x0
                       #:x1 x1
                       #:t-final t-final
                       #:cfl cfl
                      #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/isothermal_euler_validate.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-validate)))