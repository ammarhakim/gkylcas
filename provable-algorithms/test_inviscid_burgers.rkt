#lang racket

(require "code_generator_core.rkt")
(require "prover_core.rkt")
(provide (all-from-out "code_generator_core.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1D inviscid Burgers' equation: du/dt + u du/dx = 0.
(define pde-inviscid-burgers
  (hash
   'name "inviscid-burgers"
   'cons-expr `u             ; conserved variable: u
   'flux-expr `(* 0.5 u u)   ; flux function: f(u) = 0.5 * u^2
   'max-speed-expr `(abs u)  ; local wave-speed: alpha = |u|
   'parameters `()
   ))

;; Define simulation parameters.
(define nx 200)
(define x0 -1.0)
(define x1 1.0)
(define t-final 0.5)
(define cfl 0.95)
(define init-func `(cond
                     [(< x 0.0) 1.0]
                     [else 0.0]))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-lax
  (generate-lax-friedrichs-scalar-1d pde-inviscid-burgers
                                     #:nx nx
                                     #:x0 x0
                                     #:x1 x1
                                     #:t-final t-final
                                     #:cfl cfl
                                     #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_lax.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lax)))

(display "Lax-Friedrichs (finite-difference) properties: \n\n")

;; Attempt to prove hyperbolicity of the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lax-hyperbolicity
  (call-with-output-file "proofs/proof_inviscid_burgers_lax_hyperbolicity.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-lax-friedrichs-scalar-1d-hyperbolicity pde-inviscid-burgers
                                                      #:nx nx
                                                      #:x0 x0
                                                      #:x1 x1
                                                      #:t-final t-final
                                                      #:cfl cfl
                                                      #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_inviscid_burgers_lax_hyperbolicity.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-inviscid-burgers-lax-hyperbolicity)
(display "\n")

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lax-cfl-stability
  (call-with-output-file "proofs/proof_inviscid_burgers_lax_cfl_stability.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-lax-friedrichs-scalar-1d-cfl-stability pde-inviscid-burgers
                                                      #:nx nx
                                                      #:x0 x0
                                                      #:x1 x1
                                                      #:t-final t-final
                                                      #:cfl cfl
                                                      #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_inviscid_burgers_lax_cfl_stability.rkt")

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-inviscid-burgers-lax-cfl-stability)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-lax-local-lipschitz
  (call-with-output-file "proofs/proof_inviscid_burgers_lax_local_lipschitz.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-lax-friedrichs-scalar-1d-local-lipschitz pde-inviscid-burgers
                                                        #:nx nx
                                                        #:x0 x0
                                                        #:x1 x1
                                                        #:t-final t-final
                                                        #:cfl cfl
                                                        #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_inviscid_burgers_lax_local_lipschitz.rkt")

;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-inviscid-burgers-lax-local-lipschitz)
(display "\n\n\n")

;; Synthesize the code for a Roe solver for the 1D inviscid Burgers' equation.
(define code-inviscid-burgers-roe
  (generate-roe-scalar-1d pde-inviscid-burgers
                          #:nx nx
                          #:x0 x0
                          #:x1 x1
                          #:t-final t-final
                          #:cfl cfl
                          #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_roe.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-roe)))

(display "Roe (finite-volume) properties: \n\n")

;; Attempt to prove hyperbolicity of the Roe solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-roe-hyperbolicity
  (call-with-output-file "proofs/proof_inviscid_burgers_roe_hyperbolicity.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-roe-scalar-1d-hyperbolicity pde-inviscid-burgers
                                           #:nx nx
                                           #:x0 x0
                                           #:x1 x1
                                           #:t-final t-final
                                           #:cfl cfl
                                           #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_inviscid_burgers_roe_hyperbolicity.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-inviscid-burgers-roe-hyperbolicity)
(display "\n")

;; Attempt to prove flux conservation (jump continuity) of the Roe solver for the 1D inviscid Burgers' equation.
(define proof-inviscid-burgers-roe-flux-conservation
  (call-with-output-file "proofs/proof_inviscid_burgers_roe_flux_conservation.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-roe-scalar-1d-flux-conservation pde-inviscid-burgers
                                               #:nx nx
                                               #:x0 x0
                                               #:x1 x1
                                               #:t-final t-final
                                               #:cfl cfl
                                               #:init-func init-func)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_inviscid_burgers_roe_flux_conservation.rkt")

;; Show whether flux conservation (jump continuity) is preserved.
(display "Flux conservation (jump continuity): ")
(display proof-inviscid-burgers-roe-flux-conservation)
(display "\n")

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D inviscid Burgers' equation (with a second-order flux extrapolation using the minmod flux limiter).
(define code-inviscid-burgers-lax-minmod
  (generate-lax-friedrichs-scalar-1d-second-order pde-inviscid-burgers limiter-minmod
                                                  #:nx nx
                                                  #:x0 x0
                                                  #:x1 x1
                                                  #:t-final t-final
                                                  #:cfl cfl
                                                  #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_lax_minmod.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-lax-minmod)))

;; Synthesize the code for a Roe solver for the 1D inviscid Burgers' equation (with a second-order flux extrapolation using the minmod flux limiter).
(define code-inviscid-burgers-roe-minmod
  (generate-roe-scalar-1d-second-order pde-inviscid-burgers limiter-minmod
                                       #:nx nx
                                       #:x0 x0
                                       #:x1 x1
                                       #:t-final t-final
                                       #:cfl cfl
                                       #:init-func init-func))

;; Output the code to a file.
(with-output-to-file "code/inviscid_burgers_roe_minmod.c"
  #:exists 'replace
  (lambda ()
    (display code-inviscid-burgers-roe-minmod)))