#lang racket

(require "code_generator_core.rkt")
(require "code_generator_vector.rkt")
(require "prover_core.rkt")
(require "prover_vector.rkt")
(provide (all-from-out "code_generator_core.rkt"))
(provide (all-from-out "code_generator_vector.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1D Maxwell equations (Ey and Bz components).
(define pde-system-maxwell-1d-Ey-Bz
  (hash
    'name "maxwell-1d-Ey-Bz"
    'cons-exprs (list
                 `Ey
                 `Bz)                   ; conserved variables: electric field (y-component), magnetic field (z-component)
    'flux-exprs (list
                 `(* (* c c) Bz)
                 `Ey)                   ; flux vector
    'max-speed-exprs (list
                      `(abs c)
                      `(abs c))         ; local wave-speeds
    'parameters (list
                 `(define c 1.0)        ; speed of light: c = 1.0
                 `(define e_fact 1.0)   ; electric field divergence error propagation: e_fact = 1.0
                 `(define b_fact 1.0))  ; magnetic field divergence error propagation: b_fact = 1.0
    ))

;; Define simulation parameters.
(define nx 200)
(define x0 -1.5)
(define x1 1.5)
(define t-final 1.0)
(define cfl 0.95)
(define init-funcs (list
                    0.0
                    `(cond
                       [(< x 0.0) 0.5]
                       [else -0.5])))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components).
(define code-maxwell-1d-Ey-Bz-lax
  (generate-lax-friedrichs-vector2-1d pde-system-maxwell-1d-Ey-Bz
                                      #:nx nx
                                      #:x0 x0
                                      #:x1 x1
                                      #:t-final t-final
                                      #:cfl cfl
                                      #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/maxwell_1d_Ey_Bz_lax.c"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-Ey-Bz-lax)))

(display "Lax-Friedrichs (finite-difference) properties: \n\n")

;; Attempt to prove hyperbolicity of the Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components).
(define proof-maxwell-1d-Ey-Bz-lax-hyperbolicity
  (call-with-output-file "proofs/proof_maxwell_1d_Ey_Bz_lax_hyperbolicity.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-hyperbolicity pde-system-maxwell-1d-Ey-Bz
                                                       #:nx nx
                                                       #:x0 x0
                                                       #:x1 x1
                                                       #:t-final t-final
                                                       #:cfl cfl
                                                       #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_maxwell_1d_Ey_Bz_lax_hyperbolicity.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-maxwell-1d-Ey-Bz-lax-hyperbolicity)
(display "\n")

;; Attempt to prove strict hyperbolicity of the Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components).
(define proof-maxwell-1d-Ey-Bz-lax-strict-hyperbolicity
  (call-with-output-file "proofs/proof_maxwell_1d_Ey_Bz_lax_strict_hyperbolicity.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-strict-hyperbolicity pde-system-maxwell-1d-Ey-Bz
                                                              #:nx nx
                                                              #:x0 x0
                                                              #:x1 x1
                                                              #:t-final t-final
                                                              #:cfl cfl
                                                              #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_maxwell_1d_Ey_Bz_lax_strict_hyperbolicity.rkt")

;; Show whether strict hyperbolicity is preserved.
(display "Strict hyperbolicity preservation: ")
(display proof-maxwell-1d-Ey-Bz-lax-strict-hyperbolicity)
(display "\n")

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components).
(define proof-maxwell-1d-Ey-Bz-lax-cfl-stability
  (call-with-output-file "proofs/proof_maxwell_1d_Ey_Bz_lax_cfl_stability.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-cfl-stability pde-system-maxwell-1d-Ey-Bz
                                                       #:nx nx
                                                       #:x0 x0
                                                       #:x1 x1
                                                       #:t-final t-final
                                                       #:cfl cfl
                                                       #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_maxwell_1d_Ey_Bz_lax_cfl_stability.rkt")

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-maxwell-1d-Ey-Bz-lax-cfl-stability)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components)
(define proof-maxwell-1d-Ey-Bz-lax-local-lipschitz
  (call-with-output-file "proofs/proof_maxwell_1d_Ey_Bz_lax_local_lipschitz.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-lax-friedrichs-vector2-1d-local-lipschitz pde-system-maxwell-1d-Ey-Bz
                                                         #:nx nx
                                                         #:x0 x0
                                                         #:x1 x1
                                                         #:t-final t-final
                                                         #:cfl cfl
                                                         #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_maxwell_1d_Ey_Bz_lax_local_lipschitz.rkt")

;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-maxwell-1d-Ey-Bz-lax-local-lipschitz)
(display "\n\n\n")

;; Synthesize the code for a Roe solver for the 1D Maxwell equations (Ey and Bz components).
(define code-maxwell-1d-Ey-Bz-roe
  (generate-roe-vector2-1d pde-system-maxwell-1d-Ey-Bz
                           #:nx nx
                           #:x0 x0
                           #:x1 x1
                           #:t-final t-final
                           #:cfl cfl
                           #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/maxwell_1d_Ey_Bz_roe.c"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-Ey-Bz-roe)))

(display "Roe (finite-volume) properties: \n\n")

;; Attempt to prove hyperbolicity of the Roe solver for the 1D Maxwell equations (Ey and Bz components).
(define proof-maxwell-1d-Ey-Bz-roe-hyperbolicity
  (call-with-output-file "proofs/proof_maxwell_1d_Ey_Bz_roe_hyperbolicity.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-roe-vector2-1d-hyperbolicity pde-system-maxwell-1d-Ey-Bz
                                            #:nx nx
                                            #:x0 x0
                                            #:x1 x1
                                            #:t-final t-final
                                            #:cfl cfl
                                            #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_maxwell_1d_Ey_Bz_roe_hyperbolicity.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-maxwell-1d-Ey-Bz-roe-hyperbolicity)
(display "\n")

;; Attempt to prove strict hyperbolicity of the Roe solver for the 1D Maxwell equations (Ey and Bz components).
(define proof-maxwell-1d-Ey-Bz-roe-strict-hyperbolicity
  (call-with-output-file "proofs/proof_maxwell_1d_Ey_Bz_roe_strict_hyperbolicity.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-roe-vector2-1d-strict-hyperbolicity pde-system-maxwell-1d-Ey-Bz
                                                   #:nx nx
                                                   #:x0 x0
                                                   #:x1 x1
                                                   #:t-final t-final
                                                   #:cfl cfl
                                                   #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_maxwell_1d_Ey_Bz_roe_strict_hyperbolicity.rkt")

;; Show whether strict hyperbolicity is preserved.
(display "Strict hyperbolicity preservation: ")
(display proof-maxwell-1d-Ey-Bz-roe-strict-hyperbolicity)
(display "\n")

;; Attempt to prove flux conservation (jump continuity) of the Roe solver for the 1D Maxwell equations (Ey and Bz components).
(define proof-maxwell-1d-Ey-Bz-roe-flux-conservation
  (call-with-output-file "proofs/proof_maxwell_1d_Ey_Bz_roe_flux_conservation.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-roe-vector2-1d-flux-conservation pde-system-maxwell-1d-Ey-Bz
                                                #:nx nx
                                                #:x0 x0
                                                #:x1 x1
                                                #:t-final t-final
                                                #:cfl cfl
                                                #:init-funcs init-funcs)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_maxwell_1d_Ey_Bz_roe_flux_conservation.rkt")

;; Show whether flux conservation (jump continuity) is preserved.
(display "Flux conservation (jump continuity): ")
(display proof-maxwell-1d-Ey-Bz-roe-flux-conservation)
(display "\n")

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

;; Synthesize the code for a Lax-Friedrichs solver for the 1D Maxwell equations (Ey and Bz components, with a second-order flux extrapolation using the minmod flux limiter).
(define code-maxwell-1d-Ey-Bz-lax-minmod
  (generate-lax-friedrichs-vector2-1d-second-order pde-system-maxwell-1d-Ey-Bz limiter-minmod
                                                   #:nx nx
                                                   #:x0 x0
                                                   #:x1 x1
                                                   #:t-final t-final
                                                   #:cfl cfl
                                                   #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/maxwell_1d_Ey_Bz_lax_minmod.c"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-Ey-Bz-lax-minmod)))

;; Synthesize the code for a Roe solver for the 1D Maxwell equations (Ey and Bz components, with a second-order flux extrapolation using the minmod flux limiter).
(define code-maxwell-1d-Ey-Bz-roe-minmod
  (generate-roe-vector2-1d-second-order pde-system-maxwell-1d-Ey-Bz limiter-minmod
                                        #:nx nx
                                        #:x0 x0
                                        #:x1 x1
                                        #:t-final t-final
                                        #:cfl cfl
                                        #:init-funcs init-funcs))

;; Output the code to a file.
(with-output-to-file "code/maxwell_1d_Ey_Bz_roe_minmod.c"
  #:exists 'replace
  (lambda ()
    (display code-maxwell-1d-Ey-Bz-roe-minmod)))