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
(define conds-lax (list
                   ;; Sufficient conditions for guaranteeing local Lipschitz continuity of the discrete flux function.
                   `(> rho 0.0)
                   `(> (+ (* 0.5 (- (* 2.0 (/ (* mom_x mom_x) (* rho (* rho rho))))
                                    (sqrt (+ (* 8.0 (/ (* mom_x mom_x) (* rho (* rho (* rho rho)))))
                                             (+ (* 4.0 (/ (* mom_x (* mom_x (* mom_x mom_x))) (* rho (* rho (* rho (* rho (* rho rho))))))) (/ 4.0 (* rho rho)))))))
                          (/ 1.0 rho)) 0.0)
                   ))

;; Define machine epsilon.
(define epsilon `(expt 10.0 -8.0))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
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

(display "Conditional Lax-Friedrichs (finite-difference) properties: \n\n")

;; Attempt to prove hyperbolicity of the Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define proof-isothermal-euler-lax-hyperbolicity-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_lax_hyperbolicity_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-hyperbolicity-conditional pde-system-isothermal-euler conds-lax
                                                                   #:nx nx
                                                                   #:x0 x0
                                                                   #:x1 x1
                                                                   #:t-final t-final
                                                                   #:cfl cfl
                                                                   #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_lax_hyperbolicity_conditional.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-isothermal-euler-lax-hyperbolicity-conditional)
(display "\n")

;; Attempt to prove strict hyperbolicity of the Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define proof-isothermal-euler-lax-strict-hyperbolicity-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_lax_strict_hyperbolicity_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-strict-hyperbolicity-conditional pde-system-isothermal-euler conds-lax
                                                                          #:nx nx
                                                                          #:x0 x0
                                                                          #:x1 x1
                                                                          #:t-final t-final
                                                                          #:cfl cfl
                                                                          #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_lax_strict_hyperbolicity_conditional.rkt")

;; Show whether strict hyperbolicity is preserved.
(display "Strict hyperbolicity preservation: ")
(display proof-isothermal-euler-lax-strict-hyperbolicity-conditional)
(display "\n")

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define proof-isothermal-euler-lax-cfl-stability-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_lax_cfl_stability_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-cfl-stability-conditional pde-system-isothermal-euler conds-lax
                                                                   #:nx nx
                                                                   #:x0 x0
                                                                   #:x1 x1
                                                                   #:t-final t-final
                                                                   #:cfl cfl
                                                                   #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_lax_cfl_stability_conditional.rkt")

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-isothermal-euler-lax-cfl-stability-conditional)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components)
;; subject to certain algebraic constraints.
(define proof-isothermal-euler-lax-local-lipschitz-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_lax_local_lipschitz_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
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

;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-isothermal-euler-lax-local-lipschitz-conditional)
(display "\n\n\n")

;; Define algebraic constraints for the Roe solver.
(define conds-roe (list
                   ;; Sufficient conditon for guaranteeing hyperbolicity and strict hyperbolicity.
                   `(> (+ (* -2.0 (/ (* mom_xL mom_xL) (* rhoL rhoL)))
                          (+ (* 4.0 (* vt vt)) (+ (* -2.0 (/ (* mom_xR mom_xR) (* rhoR rhoR)))
                                                  (+ (* (+ (/ mom_xL rhoL) (/ mom_xR rhoR)) (/ mom_xL rhoL)) (* (+ (/ mom_xL rhoL) (/ mom_xR rhoR)) (/ mom_xR rhoR)))))) 0.0)
                   
                   ;; Sufficient condition for guaranteeing flux conservation (jump continuity). [Too strong for practical simulations!]
                   `(equal? (+ (* (+ (* -0.5 (/ (* mom_xL mom_xL) (* rhoL rhoL))) (+ (* vt vt) (* -0.5 (/ (* mom_xR mom_xR) (* rhoR rhoR))))) (- rhoL rhoR))
                               (* (+ (/ mom_xL rhoL) (/ mom_xR rhoR)) (- mom_xL mom_xR)))
                            (- (+ (/ (* mom_xL mom_xL) rhoL) (* rhoL (* vt vt))) (+ (/ (* mom_xR mom_xR) rhoR) (* rhoR (* vt vt)))))
                   ))

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

(display "Conditional Roe (finite-volume) properties: \n\n")

;; Attempt to prove hyperbolicity of the Roe solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define proof-isothermal-euler-roe-hyperbolicity-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_roe_hyperbolicity_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
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

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-isothermal-euler-roe-hyperbolicity-conditional)
(display "\n")

;; Attempt to prove strict hyperbolicity of the Roe solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define proof-isothermal-euler-roe-strict-hyperbolicity-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_roe_strict_hyperbolicity_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-roe-vector2-1d-strict-hyperbolicity-conditional pde-system-isothermal-euler conds-roe
                                                               #:nx nx
                                                               #:x0 x0
                                                               #:x1 x1
                                                               #:t-final t-final
                                                               #:cfl cfl
                                                               #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_roe_strict_hyperbolicity_conditional.rkt")

;; Show whether strict hyperbolicity is preserved.
(display "Strict hyperbolicity preservation: ")
(display proof-isothermal-euler-roe-strict-hyperbolicity-conditional)
(display "\n")

;; Attempt to prove flux conservation (jump continuity) of the Roe solver for the 1D isothermal Euler equations (density and x-momentum components) subject to certain algebraic constraints.
(define proof-isothermal-euler-roe-flux-conservation-conditional
  (call-with-output-file "proofs/proof_isothermal_euler_roe_flux_conservation_conditional.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n")
        (display "(require \"../prover_vector_conditional.rkt\")\n\n")
        (prove-roe-vector2-1d-flux-conservation-conditional pde-system-isothermal-euler conds-roe
                                                            #:nx nx
                                                            #:x0 x0
                                                            #:x1 x1
                                                            #:t-final t-final
                                                            #:cfl cfl
                                                            #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_isothermal_euler_roe_flux_conservation_conditional.rkt")

;; Show whether flux conservation (jump continuity) is preserved.
(display "Flux conservation (jump continuity): ")
(display proof-isothermal-euler-roe-flux-conservation-conditional)
(display "\n")

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D isothermal Euler equations (density and x-momentum components, with a second-order flux extrapolation using the minmod flux limiter)
;; subject to certain algebraic constraints.
(define code-isothermal-euler-lax-minmod-conditional
  (generate-lax-friedrichs-vector2-1d-second-order-conditional pde-system-isothermal-euler limiter-minmod conds-lax epsilon
                                                               #:nx nx
                                                               #:x0 x0
                                                               #:x1 x1
                                                               #:t-final t-final
                                                               #:cfl cfl
                                                               #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/isothermal_euler_lax_minmod_conditional.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-lax-minmod-conditional)))

;; Synthesize the code for a Roe solver for the 1D isothermal Euler equations (density and x-momentum components, with a second-order flux extrapolation using the minmod flux limiter)
;; subject to certain algebraic constraints.
(define code-isothermal-euler-roe-minmod-conditional
  (generate-roe-vector2-1d-second-order-conditional pde-system-isothermal-euler limiter-minmod conds-roe epsilon
                                                    #:nx nx
                                                    #:x0 x0
                                                    #:x1 x1
                                                    #:t-final t-final
                                                    #:cfl cfl
                                                    #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/isothermal_euler_roe_minmod_conditional.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-roe-minmod-conditional)))