#lang racket

(require "code_generator_core.rkt")
(require "code_generator_vector_2d_conditional.rkt")
(require "prover_vector_conditional.rkt")

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 2D isothermal Euler equations (density, x-momentum and y-momentum components).
(define pde-system-isothermal-euler-2d
  (hash
   'name "isothermal-euler-2d"
   'cons-exprs (list
                `rho
                `mom_x
                `mom_y)                                        ; conserved variables: density, x-momentum, y-momentum
   'flux-exprs-x (list
                  `mom_x
                  `(+ (/ (* mom_x mom_x) rho) (* rho vt vt))
                  `(* mom_y (/ mom_x rho)))                    ; x-flux vector
   'flux-exprs-y (list
                  `mom_y
                  `(* mom_x (/ mom_y rho))
                  `(+ (/ (* mom_y mom_y) rho) (* rho vt vt)))  ; y-flux vector
   'max-speed-exprs-x (list
                       `(abs (- (/ mom_x rho) vt))
                       `(abs (/ mom_x rho))
                       `(abs (+ (/ mom_x rho) vt)))            ; local wave-speeds (x-direction)
   'max-speed-exprs-y (list
                       `(abs (- (/ mom_y rho) vt))
                       `(abs (/ mom_y rho))
                       `(abs (+ (/ mom_y rho) vt)))            ; local wave-speeds (y-direction)
   'parameters (list
                `(define vt 1.0))                              ; thermal velocity: vt = 1.0
   ))

;; Define 2D simulation parameters.
(define nx-2d 50)
(define ny-2d 50)
(define x0-2d 0.0)
(define x1-2d 2.0)
(define y0-2d 0.0)
(define y1-2d 2.0)
(define t-final-2d 0.2)
(define cfl-2d 0.9)
(define init-funcs-2d (list
                       `(cond
                          [(> y 1.0)
                           (cond
                             [(< x 1.0) 0.5323]
                             [else 1.5])]
                          [else
                           (cond
                             [(< x 1.0) 0.138]
                             [else 0.5323])])
                       `(cond
                          [(> y 1.0)
                           (cond
                             [(< x 1.0) 1.206]
                             [else 0.0])]
                          [else
                           (cond
                             [(< x 1.0) 1.206]
                             [else 0.0])])
                       `(cond
                          [(> y 1.0)
                           (cond
                             [(< x 1.0) 0.0]
                             [else 0.0])]
                          [else
                           (cond
                             [(< x 1.0) 1.206]
                             [else 1.206])])))

;; Define algebraic constraints for the Lax-Friedrichs solver.
(define conds-lax (list `(> rho 0.0)))

;; Define machine epsilon.
(define epsilon `(expt 10.0 -8.0))

;; Synthesize the code for a Lax-Friedrichs solver for the 2D isothermal Euler equations subject to certain algebraic constraints.
(define code-isothermal-euler-lax-2d-conditional
  (generate-lax-friedrichs-vector3-2d-conditional pde-system-isothermal-euler-2d conds-lax epsilon
                                                  #:nx nx-2d
                                                  #:ny ny-2d
                                                  #:x0 x0-2d
                                                  #:x1 x1-2d
                                                  #:y0 y0-2d
                                                  #:y1 y1-2d
                                                  #:t-final t-final-2d
                                                  #:cfl cfl-2d
                                                  #:init-funcs init-funcs-2d))

;; Output the code to a file.
(with-output-to-file "code/isothermal_euler_lax_2d_conditional.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-lax-2d-conditional)))

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 2D isothermal Euler equations
;; subject to certain algebraic constraints.
(define proof-isothermal-euler-lax-local-lipschitz-2d-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_lax_local_lipschitz_2d_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector3-2d-local-lipschitz-conditional pde-system-isothermal-euler-2d conds-lax
                                                                     #:nx nx-2d
                                                                     #:ny ny-2d
                                                                     #:x0 x0-2d
                                                                     #:x1 x1-2d
                                                                     #:y0 y0-2d
                                                                     #:y1 y1-2d
                                                                     #:t-final t-final-2d
                                                                     #:cfl cfl-2d
                                                                     #:init-funcs init-funcs-2d)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_lax_local_lipschitz_2d_conditional.rkt")