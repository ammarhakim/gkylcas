#lang racket

(require "code_generator_core.rkt")
(require "code_generator_core_2d.rkt")
(require "prover_core.rkt")
(provide (all-from-out "code_generator_core.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the 1x1v electrostatic Vlasov equation: df/dt + d(vf)/dx + d(Ef)/dv = 0.
(define pde-es-vlasov-1x1v
  (hash
   'name "es-vlasov-1x1v"
   'cons-expr `fs                                             ; conserved variable: fs
   'flux-expr-x `(* y fs)                                     ; x-flux function: f(u) = y * fs
   'flux-expr-y `(* (/ qs ms) (- 0.0 (* (sin x) fs)))         ; y-flux function: f(u) = (qs / ms) * (-sin(x) * fs)
   'max-speed-expr-x `(abs y)                                 ; local x wave-speed: alpha_x = |y|
   'max-speed-expr-y `(abs (* (/ qs ms) (- 0.0 (sin x))))     ; local y wave-speed: alpha_y = |-sin(x)|
   'parameters (list
                `(define qs -1.0)                             ; species charge: qs = -1.0
                `(define ms 1.0))                             ; species mass: ms = 1.0
   ))

;; Define 1x1v simulation parameters.
(define nx-2d 100)
(define ny-2d 100)
(define x0-2d 0.0)
(define x1-2d (* 2.0 pi))
(define y0-2d -3.0)
(define y1-2d 3.0)
(define t-final-2d 3.0)
(define cfl-2d 0.95)
(define init-func-2d `(* (/ 1.0 (sqrt (* 2.0 pi))) (expt (/ (- 0.0 (* y y)) 2.0))))

;; Synthesize the code for a Lax-Friedrichs solver for the 1x1v electrostatic Vlasov equation.
(define code-es-vlasov-lax-1x1v
  (generate-lax-friedrichs-scalar-2d pde-es-vlasov-1x1v
                                     #:nx nx-2d
                                     #:ny ny-2d
                                     #:x0 x0-2d
                                     #:x1 x1-2d
                                     #:y0 y0-2d
                                     #:y1 y1-2d
                                     #:t-final t-final-2d
                                     #:cfl cfl-2d
                                     #:init-func init-func-2d))

;; Output the code to a file.
(with-output-to-file "code/es_vlasov_lax_1x1v.c"
  #:exists 'replace
  (lambda ()
    (display code-es-vlasov-lax-1x1v)))

(display "1x1v Lax-Friedrichs (finite-difference) properties: \n\n")

;; Attempt to prove hyperbolicity of the Lax-Friedrichs solver for the 1x1v electrostatic Vlasov equation.
(define proof-es-vlasov-lax-hyperbolicity-1x1v
  (call-with-output-file "proofs/proof_es_vlasov_lax_hyperbolicity_1x1v.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n\n")
        (prove-lax-friedrichs-scalar-2d-hyperbolicity pde-es-vlasov-1x1v
                                                      #:nx nx-2d
                                                      #:ny ny-2d
                                                      #:x0 x0-2d
                                                      #:x1 x1-2d
                                                      #:y0 y0-2d
                                                      #:y1 y1-2d
                                                      #:t-final t-final-2d
                                                      #:cfl cfl-2d
                                                      #:init-func init-func-2d)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_es_vlasov_lax_hyperbolicity_1x1v.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-es-vlasov-lax-hyperbolicity-1x1v)
(display "\n")

;; Attempt to prove CFL stability of the Lax-Friedrichs solver for the 1x1v electrostatic Vlasov equation.
(define proof-es-vlasov-lax-cfl-stability-1x1v
  (call-with-output-file "proofs/proof_es_vlasov_lax_cfl_stability_1x1v.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n\n")
        (prove-lax-friedrichs-scalar-2d-cfl-stability pde-es-vlasov-1x1v
                                                      #:nx nx-2d
                                                      #:ny ny-2d
                                                      #:x0 x0-2d
                                                      #:x1 x1-2d
                                                      #:y0 y0-2d
                                                      #:y1 y1-2d
                                                      #:t-final t-final-2d
                                                      #:cfl cfl-2d
                                                      #:init-func init-func-2d)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_es_vlasov_lax_cfl_stability_1x1v.rkt")

;; Show whether CFL stability is satisfied.
(display "CFL stability: ")
(display proof-es-vlasov-lax-cfl-stability-1x1v)
(display "\n")

;; Attempt to prove local Lipschitz continuity of the discrete flux function for the Lax-Friedrichs solver for the 1x1v electrostatic Vlasov equation.
(define proof-es-vlasov-lax-local-lipschitz-1x1v
  (call-with-output-file "proofs/proof_es_vlasov_lax_local_lipschitz_1x1v.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n\n")
        (prove-lax-friedrichs-scalar-2d-local-lipschitz pde-es-vlasov-1x1v
                                                        #:nx nx-2d
                                                        #:ny ny-2d
                                                        #:x0 x0-2d
                                                        #:x1 x1-2d
                                                        #:y0 y0-2d
                                                        #:y1 y1-2d
                                                        #:t-final t-final-2d
                                                        #:cfl cfl-2d
                                                        #:init-func init-func-2d)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_es_vlasov_lax_local_lipschitz_1x1v.rkt")

;; Show whether the local Lipschitz continuity property of the discrete flux function is satisfied.
(display "Local Lipschitz continuity of discrete flux function: ")
(display proof-es-vlasov-lax-local-lipschitz-1x1v)
(display "\n\n\n")

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

;; Synthesize the code for a Roe solver for the 1x1v electrostatic Vlasov equation.
(define code-es-vlasov-roe-1x1v
  (generate-roe-scalar-2d pde-es-vlasov-1x1v
                          #:nx nx-2d
                          #:ny ny-2d
                          #:x0 x0-2d
                          #:x1 x1-2d
                          #:y0 y0-2d
                          #:y1 y1-2d
                          #:t-final t-final-2d
                          #:cfl cfl-2d
                          #:init-func init-func-2d))

;; Output the code to a file.
(with-output-to-file "code/es_vlasov_roe_1x1v.c"
  #:exists 'replace
  (lambda ()
    (display code-es-vlasov-roe-1x1v)))

(display "1x1v Roe (finite-volume) properties: \n\n")

;; Attempt to prove hyperbolicity of the Roe solver for the 1x1v electrostatic Vlasov equation.
(define proof-es-vlasov-roe-hyperbolicity-1x1v
  (call-with-output-file "proofs/proof_es_vlasov_roe_hyperbolicity_1x1v.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n\n")
        (prove-roe-scalar-2d-hyperbolicity pde-es-vlasov-1x1v
                                           #:nx nx-2d
                                           #:ny ny-2d
                                           #:x0 x0-2d
                                           #:x1 x1-2d
                                           #:y0 y0-2d
                                           #:y1 y1-2d
                                           #:t-final t-final-2d
                                           #:cfl cfl-2d
                                           #:init-func init-func-2d)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_es_vlasov_roe_hyperbolicity_1x1v.rkt")

;; Show whether hyperbolicity is preserved.
(display "Hyperbolicity preservation: ")
(display proof-es-vlasov-roe-hyperbolicity-1x1v)
(display "\n")

;; Synthesize the code for a Lax-Friedrichs solver for the 1x1v electrostatic Vlasov equation (with a second-order flux extrapolation using the minmod flux limiter).
(define code-es-vlasov-lax-minmod-1x1v
  (generate-lax-friedrichs-scalar-2d-second-order pde-es-vlasov-1x1v limiter-minmod
                                                  #:nx nx-2d
                                                  #:ny ny-2d
                                                  #:x0 x0-2d
                                                  #:x1 x1-2d
                                                  #:y0 y0-2d
                                                  #:y1 y1-2d
                                                  #:t-final t-final-2d
                                                  #:cfl cfl-2d
                                                  #:init-func init-func-2d))

;; Output the code to a file.
(with-output-to-file "code/es_vlasov_lax_minmod_1x1v.c"
  #:exists 'replace
  (lambda ()
    (display code-es-vlasov-lax-minmod-1x1v)))

;; Synthesize the code for a Roe solver for the 1x1v electrostatic Vlasov equation (with a second-order flux extrapolation using the minmod flux limiter).
(define code-es-vlasov-roe-minmod-1x1v
  (generate-roe-scalar-2d-second-order pde-es-vlasov-1x1v limiter-minmod
                                       #:nx nx-2d
                                       #:ny ny-2d
                                       #:x0 x0-2d
                                       #:x1 x1-2d
                                       #:y0 y0-2d
                                       #:y1 y1-2d
                                       #:t-final t-final-2d
                                       #:cfl cfl-2d
                                       #:init-func init-func-2d))

;; Output the code to a file.
(with-output-to-file "code/es_vlasov_roe_minmod_1x1v.c"
  #:exists 'replace
  (lambda ()
    (display code-es-vlasov-roe-minmod-1x1v)))

;; Attempt to prove flux conservation (jump continuity) of the Roe solver for the 1x1v electrostatic Vlasov equation.
(define proof-es-vlasov-roe-flux-conservation-1x1v
  (call-with-output-file "proofs/proof_es_vlasov_roe_flux_conservation_1x1v.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover_core.rkt\")\n\n")
        (prove-roe-scalar-2d-flux-conservation pde-es-vlasov-1x1v
                                               #:nx nx-2d
                                               #:ny ny-2d
                                               #:x0 x0-2d
                                               #:x1 x1-2d
                                               #:y0 y0-2d
                                               #:y1 y1-2d
                                               #:t-final t-final-2d
                                               #:cfl cfl-2d
                                               #:init-func init-func-2d)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_es_vlasov_roe_flux_conservation_1x1v.rkt")

;; Show whether flux conservation (jump continuity) is preserved.
(display "Flux conservation (jump continuity): ")
(display proof-es-vlasov-roe-flux-conservation-1x1v)
(display "\n")