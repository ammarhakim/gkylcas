#lang racket

(require "geometry_code_generator_core.rkt")
(require "geometry_prover_core.rkt")
(provide (all-from-out "geometry_code_generator_core.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

(define geometry-cylindrical
  (hash
   'name "gk-cylindrical"
   'exprs (list `(* (R psi theta) (cos (phi psi theta alpha)))
                `(* (R psi theta) (sin (phi psi theta alpha)))
                `(Z psi theta))
   'coords (list `psi `theta `alpha)
   'func-exprs (list
                `(R psi theta)
                `(phi psi theta alpha)
                `(Z psi theta))))

(define code-gk-cylindrical-tangent-vectors
  (generate-tangent-vectors-3d geometry-cylindrical))

(with-output-to-file "code/gk_cylindrical_tangent_vectors.c"
  #:exists 'replace
  (lambda ()
    (display code-gk-cylindrical-tangent-vectors)))