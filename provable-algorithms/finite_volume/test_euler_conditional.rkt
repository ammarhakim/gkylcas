#lang racket

(require "code_generator_core.rkt")
(require "prover_core.rkt")
(require "prover_matrix_conditional.rkt")
(provide (all-from-out "code_generator_core.rkt"))

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
(define nx 200)
(define x0 0.0)
(define x1 1.0)
(define t-final 0.2)
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

;; Define algebraic constraints for the Lax-Friedrichs solver.
(define conds-lax (list
                   `(> rho 0.0)
                   `(> energy 0.0)
                   `(> (- (* gamma (- (* rho (* rho (* rho (* rho (* mom_x mom_x))))) (* gamma (* (mom_x mom_x) (* rho (* rho (* rho rho)))))))
                          (* 2.0 (* gamma (* energy (* rho (* rho (* rho (* rho rho)))))))) 0.0)))

(display "Conditional Lax-Friedrichs (finite-difference) properties: \n\n")

;; Attempt to prove hyperbolicity of the Lax-Friedrichs solver for the 1D Euler equations (density, x-momentum, and total energy components) subject to certain algebraic constraints.
(define proof-euler-lax-hyperbolicity-conditional
  (call-with-output-file "proofs/proof_euler_lax_hyperbolicity_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_matrix_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector3-1d-hyperbolicity-conditional pde-system-euler conds-lax
                                                                   #:nx nx
                                                                   #:x0 x0
                                                                   #:x1 x1
                                                                   #:t-final t-final
                                                                   #:cfl cfl
                                                                   #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_euler_lax_hyperbolicity_conditional.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-euler-lax-hyperbolicity-conditional)
(display "\n")