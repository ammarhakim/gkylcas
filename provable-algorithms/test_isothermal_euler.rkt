#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

;; Define the 1D isothermal Euler equations.
(define pde-system-isothermal-euler
  (hash
    'name "isothermal-euler"
    'cons-exprs (list
                 `rho
                 `mom)                                    ; conserved variables: density, momentum
    'flux-exprs (list
                 `mom
                 `(+ (/ (* mom mom) rho) (* rho vt vt)))  ; flux vector
    'max-speed-exprs (list
                      `(abs (- (/ mom rho) vt))
                      `(abs (+ (/ mom rho) vt)))          ; local wave-speeds
    'parameters `(define vt 1.0)                          ; thermal velocity: vt = 1.0
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

;; Synthesize the code for a Lax-Friedrichs solver for the 1D isothermal Euler equations.
(define code-isothermal-euler-lf
  (generate-lax-friedrichs-vector2-1d pde-system-isothermal-euler
                                      #:nx nx
                                      #:x0 x0
                                      #:x1 x1
                                      #:t-final t-final
                                      #:cfl cfl
                                      #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "isothermal_euler_lf.c"
  #:exists 'replace
  (lambda ()
    (display code-isothermal-euler-lf)))

;; Attempt to prove hyperbolicity of the Lax-Friedrichs solver for the 1D isothermal Euler equations.
(define proof-isothermal-euler-lf-hyperbolicity
  (call-with-output-file "proof_isothermal_euler_lf_hyperbolicity.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-vector2-1d-hyperbolicity pde-system-isothermal-euler
                                                       #:nx nx
                                                       #:x0 x0
                                                       #:x1 x1
                                                       #:t-final t-final
                                                       #:cfl cfl
                                                       #:init-funcs init-funcs)))
    #:exists `replace))

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-isothermal-euler-lf-hyperbolicity)
(display "\n")

;; Attempt to prove strict hyperbolicity of the Lax-Friedrichs solver for the 1D isothermal Euler equations.
(define proof-isothermal-euler-lf-strict-hyperbolicity
  (call-with-output-file "proof_isothermal_euler_lf_strict_hyperbolicity.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-vector2-1d-strict-hyperbolicity pde-system-isothermal-euler
                                                              #:nx nx
                                                              #:x0 x0
                                                              #:x1 x1
                                                              #:t-final t-final
                                                              #:cfl cfl
                                                              #:init-funcs init-funcs)))
    #:exists `replace))

;; Show whether strict hyperbolicity is preserved.
(display "Strict hyperbolicity preservation: ")
(display proof-isothermal-euler-lf-strict-hyperbolicity)
(display "\n")

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1D isothermal Euler equations.
(define proof-isothermal-euler-lf-cfl-stability
  (call-with-output-file "proof_isothermal_euler_lf_cfl_stability.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-vector2-1d-cfl-stability pde-system-isothermal-euler
                                                       #:nx nx
                                                       #:x0 x0
                                                       #:x1 x1
                                                       #:t-final t-final
                                                       #:cfl cfl
                                                       #:init-funcs init-funcs)))
    #:exists `replace))

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-isothermal-euler-lf-cfl-stability)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D isothermal Euler equations.
(define proof-isothermal-euler-lf-local-lipschitz
  (call-with-output-file "proof_isothermal_euler_lf_local_lipschitz.txt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (prove-lax-friedrichs-vector2-1d-local-lipschitz pde-system-isothermal-euler
                                                         #:nx nx
                                                         #:x0 x0
                                                         #:x1 x1
                                                         #:t-final t-final
                                                         #:cfl cfl
                                                         #:init-funcs init-funcs)))
    #:exists `replace))

;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-isothermal-euler-lf-local-lipschitz)
(display "\n")