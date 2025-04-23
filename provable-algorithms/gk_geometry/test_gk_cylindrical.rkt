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
                `(define (R psi theta) (sqrt (* 4.0 psi)))       ; R (function of psi and theta)
                `(define (phi psi theta alpha) (* -1.0 alpha))   ; phi (function of psi, theta, and alpha)
                `(define (Z psi theta) (/ theta pi)))))          ; Z (function of psi and theta)

;; Define domain parameters.
(define nx 8)
(define x0 0.1)
(define x1 0.2)
(define ny 8)
(define y0 (* -1.0 pi))
(define y1 pi)
(define nz 8)
(define z0 (* -1.0 pi))
(define z1 pi)

;; Synthesize the code for 3D tangent vector computation in a cylindrical GK geometry.
(define code-gk-cylindrical-tangent-vectors
  (generate-tangent-vectors-3d geometry-cylindrical
                               #:nx nx
                               #:x0 x0
                               #:x1 x1
                               #:ny ny
                               #:y0 y0
                               #:y1 y1
                               #:nz nz
                               #:z0 z0
                               #:z1 z1))

;; Output the code to a file.
(with-output-to-file "code/gk_cylindrical_tangent_vectors.c"
  #:exists 'replace
  (lambda ()
    (display code-gk-cylindrical-tangent-vectors)))