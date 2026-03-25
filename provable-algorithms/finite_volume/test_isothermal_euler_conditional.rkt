#lang racket

(require "code_generator_core.rkt")
(require "code_generator_vector_conditional.rkt")
(require "prover_vector_conditional.rkt")

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

;; Define algebraic constraints for the Lax-Friedrichs solver.
(define conds-lax (list `(>= rho 0.0)
                        `(>= (+ (* 0.5 (- (* 2.0 (/ (* mom_x mom_x) (* rho (* rho rho))))
                                          (sqrt (+ (* 8.0 (/ (* mom_x mom_x) (* rho (* rho (* rho rho)))))
                                                   (+ (* 4.0 (/ (* mom_x (* mom_x (* mom_x mom_x))) (* rho (* rho (* rho (* rho (* rho rho))))))) (/ 4.0 (* rho rho)))))))
                                (/ 1.0 rho)) 0.0)))

;; Define machine epsilon.
(define epsilon `(expt 10.0 -8.0))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components).
(define code-isothermal-euler-lax-conditional
  (generate-lax-friedrichs-vector2-1d-conditional pde-system-isothermal-euler conds-lax epsilon
                                                  #:nx nx
                                                  #:x0 x0
                                                  #:x1 x1
                                                  #:t-final t-final
                                                  #:cfl cfl
                                                  #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/isothermal_euler_lax_conditional.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-lax-conditional)))

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components)
;; subject to certain algebraic constraints.
(define proof-isothermal-euler-lax-local-lipschitz-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_lax_local_lipschitz_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-local-lipschitz-conditional pde-system-isothermal-euler conds-lax
                                                                     #:nx nx
                                                                     #:x0 x0
                                                                     #:x1 x1
                                                                     #:t-final t-final
                                                                     #:cfl cfl
                                                                     #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_lax_local_lipschitz_conditional.rkt")

;; Define algebraic constraints for the Roe solver.
(define conds-roe (list `(>= (+ (* -2.0 (/ (* mom_xL mom_xL) (* rhoL rhoL)))
                                (+ (* 4.0 (* vt vt)) (+ (* -2.0 (/ (* mom_xR mom_xR) (* rhoR rhoR)))
                                                        (+ (* (+ (/ mom_xL rhoL) (/ mom_xR rhoR)) (/ mom_xL rhoL)) (* (+ (/ mom_xL rhoL) (/ mom_xR rhoR)) (/ mom_xR rhoR)))))) 0.0)))

;; Synthesize the code for a Roe solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define code-isothermal-euler-roe-conditional
  (generate-roe-vector2-1d-conditional pde-system-isothermal-euler conds-roe epsilon
                                       #:nx nx
                                       #:x0 x0
                                       #:x1 x1
                                       #:t-final t-final
                                       #:cfl cfl
                                       #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/isothermal_euler_roe_conditional.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-roe-conditional)))

;; Attempt to prove hyperbolicity of the Roe solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define proof-isothermal-euler-roe-hyperbolicity-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_roe_hyperbolicity_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-roe-vector2-1d-hyperbolicity-conditional pde-system-isothermal-euler conds-roe
                                                        #:nx nx
                                                        #:x0 x0
                                                        #:x1 x1
                                                        #:t-final t-final
                                                        #:cfl cfl
                                                        #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_roe_hyperbolicity_conditional.rkt")