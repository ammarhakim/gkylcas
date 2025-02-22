#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide (all-from-out "code_generator.rkt"))

;; Construct /proofs output directory if it does not already exist.
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the minmod flux limiter.
(define limiter-minmod
  (hash
   'name "minmod"
   'limiter-expr `(max 0.0 (min 1.0 r))
   'limiter-ratio `r
   ))

(display "Minmod flux limiter properties: \n\n")

;; Attempt to prove second-order TVD (total variation diminishing) of the minmod flux limiter.
(define proof-limiter-minmod-tvd
  (call-with-output-file "proofs/proof_limiter_minmod_tvd.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-flux-limiter-tvd limiter-minmod)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_limiter_minmod_tvd.rkt")

;; Show whether the second-order TVD (total variation diminishing) property is satisfied.
(display "Second-order TVD (total variation diminishing): ")
(display proof-limiter-minmod-tvd)
(display "\n\n\n")

;; Define the superbee flux limiter.
(define limiter-superbee
  (hash
   'name "superbee"
   'limiter-expr `(max 0.0 (min (* 2.0 r) 1.0) (min r 2.0))
   'limiter-ratio `r
   ))

(display "Superbee flux limiter properties: \n\n")

;; Attempt to prove second-order TVD (total variation diminishing) of the superbee flux limiter.
(define proof-limiter-superbee-tvd
  (call-with-output-file "proofs/proof_limiter_superbee_tvd.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-flux-limiter-tvd limiter-superbee)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_limiter_superbee_tvd.rkt")

;; Show whether the second-order TVD (total variation diminishing) property is satisfied.
(display "Second-order TVD (total variation diminishing): ")
(display proof-limiter-superbee-tvd)
(display "\n\n\n")

;; Define the monotonized-centered flux limiter.
(define limiter-monotonized-centered
  (hash
   'name "monotonized-centered"
   'limiter-expr `(max 0.0 (min (* 2.0 r) (min (/ (+ 1.0 r) 2.0) 2.0)))
   'limiter-ratio `r
   ))

(display "Monotonized-centered flux limiter properties: \n\n")

;; Attempt to prove second-order TVD (total variation diminishing) of the monotonized-centered flux limiter.
(define proof-limiter-monotonized-centered-tvd
  (call-with-output-file "proofs/proof_limiter_monotonized_centered_tvd.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../prover.rkt\")\n\n")
        (prove-flux-limiter-tvd limiter-monotonized-centered)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/proof_limiter_monotonized_centered_tvd.rkt")

;; Show whether the second-order TVD (total variation diminishing) property is satisfied.
(display "Second-order TVD (total variation diminishing): ")
(display proof-limiter-monotonized-centered-tvd)
(display "\n\n\n")