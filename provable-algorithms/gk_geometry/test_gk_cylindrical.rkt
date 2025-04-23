#lang racket

(require "geometry_code_generator_core.rkt")
(require "geometry_prover_core.rkt")
(provide (all-from-out "geometry_code_generator_core.rkt"))

;; Construct /code and /proofs output directories if they do not already exist.
(cond
  [(not (directory-exists? "code")) (make-directory "code")])
(cond
  [(not (directory-exists? "proofs")) (make-directory "proofs")])

;; Define the cylindrical GK geometry in field-line-following coordinates.
(define geometry-cylindrical
  (hash
   'name "gk-cylindrical"
   'exprs (list `(* (R psi theta) (cos (phi psi theta alpha)))   ; x-coordinate: R * cos(phi)
                `(* (R psi theta) (sin (phi psi theta alpha)))   ; y-coordinate: R * sin(phi)
                `(Z psi theta))                                  ; z-coordinate: Z
   'coords (list `psi `theta `alpha)                             ; field-line-following coordinates: psi, theta, alpha
   'func-exprs (list
                `(R psi theta)                                   ; R (function of psi and theta)
                `(phi psi theta alpha)                           ; phi (function of psi, theta, and alpha)
                `(Z psi theta))))                                ; Z (function of psi and theta)

;; Synthesize the code for 3D tangent vector computation in a cylindrical GK geometry.
(define code-gk-cylindrical-tangent-vectors
  (generate-tangent-vectors-3d geometry-cylindrical))

;; Output the code to a file.
(with-output-to-file "code/gk_cylindrical_tangent_vectors.c"
  #:exists 'replace
  (lambda ()
    (display code-gk-cylindrical-tangent-vectors)))