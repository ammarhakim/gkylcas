#lang racket

(require "code_generator_core.rkt")
(require "code_generator_matrix_conditional.rkt")
(require "prover_core.rkt")
(require "prover_matrix_conditional.rkt")
(provide (all-from-out "code_generator_core.rkt"))
(provide (all-from-out "code_generator_matrix_conditional.rkt"))

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
                   ;; Sufficient condition for guaranteeing hyperbolicity and strict hyperbolicity.
                   `(> (+ (- (* gamma (- (* rho (* rho (* rho (* rho (* mom_x mom_x))))) (* gamma (* mom_x (* mom_x (* rho (* rho (* rho rho))))))))
                             (* 2.0 (* gamma (* energy (* rho (* rho (* rho (* rho rho)))))))) (* 2.0 (* gamma (* gamma (* rho (* energy (* rho (* rho (* rho rho))))))))) 0.0)

                   ;; Sufficient condition for guaranteeing CFL stability.
                   `(equal? (* 1.4142135623730951 (/ (sqrt (+ (- (* gamma (- (* rho (* rho (* rho (* rho (* mom_x mom_x))))) (* gamma (* mom_x (* mom_x (* rho (* rho (* rho rho))))))))
                                                                 (* 2.0 (* gamma (* energy (* rho (* rho (* rho (* rho rho))))))))
                                                              (* 2.0 (* gamma (* gamma (* rho (* energy (* rho (* rho (* rho rho)))))))))) (* 2.0 (* rho (* rho rho)))))
                            (sqrt (/ (* gamma (* (- gamma 1.0) (- energy (* 0.5 (/ (* mom_x mom_x) rho))))) rho)))

                   ;; Necessary conditions for guaranteeing local Lipschitz continuity of the discrete flux function (not sufficient).
                   `(> rho 0.0)
                   `(> (- (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                             (+ (/ (* mom_x mom_x) (* rho (* rho rho))) (+ (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))
                                                                           (+ (/ 1.0 rho) (+ (/ 1.0 rho) (* -1.0 (* (- gamma 1.0) (/ 1.0 rho))))))))
                          (sqrt (+ (- (+ (* (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                               (+ (/ (* mom_x mom_x) (* rho (* rho rho))) (+ (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))
                                                                                             (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                   (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho))))))))))
                                            (/ (* mom_x mom_x) (* rho (* rho rho))))
                                         (+ (* -1.0 (* (+ (/ (* mom_x mom_x) (* rho (* rho rho))) (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                     (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))))
                                                       (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho))))))
                                            (* 4.0 (* (+ -2.0 (- gamma 1.0)) (* (/ mom_x (* rho rho)) (* (+ -2.0 (- gamma 1.0)) (/ mom_x (* rho rho))))))))
                                      (+ (* 4.0 (* (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                      (+ (/ (* mom_x mom_x) (* rho (* rho rho))) (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))))
                                                   (/ 1.0 rho))) (* -2.0 (* (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                               (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                  (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))))
                                                                            (* (- gamma 1.0) (/ 1.0 rho))))))
                                   (+ (* (+ (/ 1.0 rho) (+ (/ 1.0 rho) (+ (* -1.0 (* (- gamma 1.0) (/ 1.0 rho))) (+ (/ 1.0 rho) (+ (/ 1.0 rho) (* -1.0 (* (- gamma 1.0) (/ 1.0 rho))))))))
                                         (/ 1.0 rho)) (* -1.0 (* (+ (/ 1.0 rho) (+ (/ 1.0 rho) (* -1.0 (* (- gamma 1.0) (/ 1.0 rho))))) (* (- gamma 1.0) (/ 1.0 rho)))))))) 0.0)
                   `(> (+ (* -0.5 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))
                          (+ (/ 0.5 rho) (+ (/ 0.5 rho) (+ (* -0.5 (* (- gamma 1.0) (/ 1.0 rho)))
                                                           (* 0.5 (sqrt (+ (- (+ (* (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                       (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                          (+ (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))
                                                                                             (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                   (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho))))))))))
                                                                                    (/ (* mom_x mom_x) (* rho (* rho rho))))
                                                                                 (+ (* -1.0 (* (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                  (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                     (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))))
                                                                                               (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho))))))
                                                                                    (* 4.0 (* (+ -2.0 (- gamma 1.0)) (* (/ mom_x (* rho rho))
                                                                                                                        (* (+ -2.0 (- gamma 1.0)) (/ mom_x (* rho rho))))))))
                                                                              (+ (* 4.0 (* (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                              (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                 (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho))))))) (/ 1.0 rho)))
                                                                                 (* -2.0 (* (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                               (+ (/ (* mom_x mom_x) (* rho (* rho rho)))
                                                                                                  (* -1.0 (* (- gamma 1.0) (/ (* mom_x mom_x) (* rho (* rho rho)))))))
                                                                                            (* (- gamma 1.0) (/ 1.0 rho))))))
                                                                           (+ (* (+ (/ 1.0 rho) (+ (/ 1.0 rho) (+ (* -1.0 (* (- gamma 1.0) (/ 1.0 rho)))
                                                                                                                  (+ (/ 1.0 rho) (+ (/ 1.0 rho) (* -1.0 (* (- gamma 1.0) (/ 1.0 rho))))))))
                                                                                 (/ 1.0 rho))
                                                                              (* -1.0 (* (+ (/ 1.0 rho) (+ (/ 1.0 rho) (* -1.0 (* (- gamma 1.0) (/ 1.0 rho)))))
                                                                                         (* (- gamma 1.0) (/ 1.0 rho)))))))))))) 0.0)
                   `(> (+ (* -3.0 (/ (* rho (* rho (* mom_x (* (- gamma 1.0) (* rho rho))))) (* 6.0 (* rho (* rho (* rho (* rho (* rho rho))))))))
                          (+ (* -3.0 (/ (* rho (* rho (* mom_x (* (- gamma 1.0) (* mom_x mom_x))))) (* 6.0 (* rho (* rho (* rho (* rho (* rho rho))))))))
                             (+ (* 2.0 (/ (* rho (* rho (* mom_x (* gamma (* rho energy))))) (* 6.0 (* rho (* rho (* rho (* rho (* rho rho))))))))
                                (/ (* rho (* rho (* (sqrt (+ (* rho rho) (* mom_x mom_x)))
                                                    (sqrt (+ (- (+ (* 9.0 (* mom_x (* mom_x (* rho rho)))) (* 9.0 (* mom_x (* mom_x (* mom_x mom_x)))))
                                                                (* 6.0 (* gamma (* mom_x (* mom_x (- (+ (* 3.0 (* rho rho)) (* 3.0 (* mom_x mom_x))) (* 2.0 (* rho energy))))))))
                                                             (+ (* gamma (* gamma (- (+ (* 4.0 (* rho (* rho (* rho rho))))
                                                                                        (* 9.0 (* mom_x (* mom_x (* mom_x mom_x))))) (* 12.0 (* rho (* energy (* mom_x mom_x)))))))
                                                                (+ (* 9.0 (* gamma (* gamma (* rho (* rho (* mom_x mom_x)))))) (* 4.0 (* gamma (* gamma (* rho (* rho (* energy energy)))))))))))))
                                   (* 6.0 (* rho (* rho (* rho (* rho (* rho rho)))))))))) 0.0)

                   ;; Sufficient condition for guaranteeing local Lipschitz continuity of the discrete flux function. [Too strong for practical simulations!]
                   `(> (- (+ (* -3.0 (/ (* rho (* rho (* mom_x (* (- gamma 1.0) (* rho rho))))) (* 6.0 (* rho (* rho (* rho (* rho (* rho rho))))))))
                             (+ (* -3.0 (/ (* rho (* rho (* mom_x (* (- gamma 1.0) (* mom_x mom_x))))) (* 6.0 (* rho (* rho (* rho (* rho (* rho rho))))))))
                                (* 2.0 (/ (* rho (* rho (* mom_x (* gamma (* rho energy))))) (* 6.0 (* rho (* rho (* rho (* rho (* rho rho))))))))))
                          (/ (* rho (* rho (* (sqrt (+ (* rho rho) (* mom_x mom_x)))
                                              (sqrt (+ (- (+ (* 9.0 (* mom_x (* mom_x (* rho rho)))) (* 9.0 (* mom_x (* mom_x (* mom_x mom_x)))))
                                                          (* 6.0 (* gamma (* mom_x (* mom_x (- (+ (* 3.0 (* rho rho)) (* 3.0 (* mom_x mom_x))) (* 2.0 (* rho energy))))))))
                                                        (+ (* gamma (* gamma (- (+ (* 4.0 (* rho (* rho (* rho rho)))) (* 9.0 (* mom_x (* mom_x (* mom_x mom_x)))))
                                                                                (* 12.0 (* rho (* energy (* mom_x mom_x)))))))
                                                           (+ (* 9.0 (* gamma (* gamma (* rho (* rho (* mom_x mom_x)))))) (* 4.0 (* gamma (* gamma (* rho (* rho (* energy energy)))))))))))))
                             (* 6.0 (* rho (* rho (* rho (* rho (* rho rho)))))))) 0.0)
                   ))

;; Define machine epsilon.
(define epsilon `(expt 10.0 -8.0))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D Euler equations (density, x-momentum, and total energy components) subject to certain algebraic constraints.
(define code-euler-lax-conditional
  (generate-lax-friedrichs-vector3-1d-conditional pde-system-euler conds-lax epsilon
                                                  #:nx nx
                                                  #:x0 x0
                                                  #:x1 x1
                                                  #:t-final t-final
                                                  #:cfl cfl
                                                  #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/euler_lax_conditional.c"
  #:exists 'replace
  (lambda ()
    (display code-euler-lax-conditional)))

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

;; Attempt to prove strict hyperbolicity of the Lax-Friedrichs solver for the 1D Euler equations (density, x-momentum, and total energy components) subject to certain algebraic constraints.
(define proof-euler-lax-strict-hyperbolicity-conditional
  (call-with-output-file "proofs/proof_euler_lax_strict_hyperbolicity_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_matrix_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector3-1d-strict-hyperbolicity-conditional pde-system-euler conds-lax
                                                                          #:nx nx
                                                                          #:x0 x0
                                                                          #:x1 x1
                                                                          #:t-final t-final
                                                                          #:cfl cfl
                                                                          #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_euler_lax_strict_hyperbolicity_conditional.rkt")

;; Show whether strict hyperbolicity is preserved.
(display "Strict hyperbolicity preservation: ")
(display proof-euler-lax-strict-hyperbolicity-conditional)
(display "\n")

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1D Euler equations (density, x-momentum, and total energy components) subject to certain algebraic constraints.
(define proof-euler-lax-cfl-stability-conditional
  (call-with-output-file "proofs/proof_euler_lax_cfl_stability_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_matrix_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector3-1d-cfl-stability-conditional pde-system-euler conds-lax
                                                                   #:nx nx
                                                                   #:x0 x0
                                                                   #:x1 x1
                                                                   #:t-final t-final
                                                                   #:cfl cfl
                                                                   #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_euler_lax_cfl_stability_conditional.rkt")

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-euler-lax-cfl-stability-conditional)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D Euler equations (density, x-momentum, and total energy components)
;; subject to certain algebraic constraints.
(define proof-euler-lax-local-lipschitz-conditional
  (call-with-output-file "proofs/proof_euler_lax_local_lipschitz_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_matrix_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector3-1d-local-lipschitz-conditional pde-system-euler conds-lax
                                                                     #:nx nx
                                                                     #:x0 x0
                                                                     #:x1 x1
                                                                     #:t-final t-final
                                                                     #:cfl cfl
                                                                     #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_euler_lax_local_lipschitz_conditional.rkt")

;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-euler-lax-local-lipschitz-conditional)
(display "\n\n\n")