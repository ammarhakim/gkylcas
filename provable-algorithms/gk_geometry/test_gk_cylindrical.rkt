#lang racket

(require "geometry_code_generator_core.rkt")
(require "geometry_prover_core.rkt")
(require "geometry_prover_metric.rkt")
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
(define x1 0.2)          ; psi ranges between 0.1 and 0.2 (8 cells).
(define ny 8)
(define y0 (* -1.0 pi))
(define y1 pi)           ; theta ranges between -pi and pi (8 cells).
(define nz 8)
(define z0 (* -1.0 pi))
(define z1 pi)           ; alpha ranges between -pi and pi (8 cells).

;; Synthesize the code for 3D tangent vector computation in a cylindrical GK geometry, using automatic differentiation.
(define code-gk-cylindrical-tangent-vector
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
    (display code-gk-cylindrical-tangent-vector)))

(display "3D tangent vector (automatic differentiation) properties:\n\n")

;; Attempt to prove finiteness of the 3D tangent vectors in a cylindrical GK geometry, using automatic differentiation.
(define proof-gk-cylindrical-tangent-vectors-3d-finite
  (call-with-output-file "proofs/gk_cylindrical_tangent_vectors_3d_finite.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-tangent-vectors-3d-finite geometry-cylindrical
                                         #:nx nx
                                         #:x0 x0
                                         #:x1 x1
                                         #:ny ny
                                         #:y0 y0
                                         #:y1 y1
                                         #:nz nz
                                         #:z0 z0
                                         #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_tangent_vectors_3d_finite.rkt")

;; Show whether 3D tangent vectors are finite.
(display "Tangent vectors finite: ")
(display proof-gk-cylindrical-tangent-vectors-3d-finite)
(display "\n")

;; Attempt to prove finiteness of the 3D tangent vectors in a cylindrical GK geometry (excluding the X-point), using automatic differentiation.
(define proof-gk-cylindrical-tangent-vectors-3d-finite-x-point
  (call-with-output-file "proofs/gk_cylindrical_tangent_vectors_3d_finite_x_point.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-tangent-vectors-3d-finite-x-point geometry-cylindrical `psi
                                                 #:nx nx
                                                 #:x0 x0
                                                 #:x1 x1
                                                 #:ny ny
                                                 #:y0 y0
                                                 #:y1 y1
                                                 #:nz nz
                                                 #:z0 z0
                                                 #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_tangent_vectors_3d_finite_x_point.rkt")

;; Show whether 3D tangent vectors are finite (excluding the X-point).
(display "Tangent vectors finite (excluding X-point): ")
(display proof-gk-cylindrical-tangent-vectors-3d-finite-x-point)
(display "\n")

;; Attempt to prove realness of the 3D tangent vectors in a cylindrical GK geometry, using automatic differentiation.
(define proof-gk-cylindrical-tangent-vectors-3d-real
  (call-with-output-file "proofs/gk_cylindrical_tangent_vectors_3d_real.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-tangent-vectors-3d-real geometry-cylindrical
                                       #:nx nx
                                       #:x0 x0
                                       #:x1 x1
                                       #:ny ny
                                       #:y0 y0
                                       #:y1 y1
                                       #:nz nz
                                       #:z0 z0
                                       #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_tangent_vectors_3d_real.rkt")

;; Show whether 3D tangent vectors are real.
(display "Tangent vectors real: ")
(display proof-gk-cylindrical-tangent-vectors-3d-real)
(display "\n")

;; Attempt to prove realness of the 3D tangent vectors in a cylindrical GK geometry (from the X-point outwards), using automatic differentiation.
(define proof-gk-cylindrical-tangent-vectors-3d-real-x-point
  (call-with-output-file "proofs/gk_cylindrical_tangent_vectors_3d_real_x_point.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-tangent-vectors-3d-real-x-point geometry-cylindrical `psi
                                               #:nx nx
                                               #:x0 x0
                                               #:x1 x1
                                               #:ny ny
                                               #:y0 y0
                                               #:y1 y1
                                               #:nz nz
                                               #:z0 z0
                                               #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_tangent_vectors_3d_real_x_point.rkt")

;; Show whether 3D tangent vectors are real (from the X-point outwards).
(display "Tangent vectors real (X-point outwards): ")
(display proof-gk-cylindrical-tangent-vectors-3d-real-x-point)
(display "\n\n\n")

;; Synthesize the code for 3D metric tensor computation in a cylindrical GK geometry, using automatic differentiation.
(define code-gk-cylindrical-metric-tensor
  (generate-metric-tensor-3d geometry-cylindrical
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
(with-output-to-file "code/gk_cylindrical_metric_tensor.c"
  #:exists 'replace
  (lambda ()
    (display code-gk-cylindrical-metric-tensor)))

(display "3D metric tensor (automatic differentiation) properties:\n\n")

;; Attempt to prove finiteness of the 3D metric tensor in a cylindrical GK geometry, using automatic differentiation.
(define proof-gk-cylindrical-metric-tensor-3d-finite
  (call-with-output-file "proofs/gk_cylindrical_metric_tensor_3d_finite.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-metric-tensor-3d-finite geometry-cylindrical
                                       #:nx nx
                                       #:x0 x0
                                       #:x1 x1
                                       #:ny ny
                                       #:y0 y0
                                       #:y1 y1
                                       #:nz nz
                                       #:z0 z0
                                       #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_metric_tensor_3d_finite.rkt")

;; Show whether 3D metric tensor is finite.
(display "Metric tensor finite: ")
(display proof-gk-cylindrical-metric-tensor-3d-finite)
(display "\n")

;; Attempt to prove finiteness of the 3D metric tensor in a cylindrical GK geometry (excluding the X-point), using automatic differentiation.
(define proof-gk-cylindrical-metric-tensor-3d-finite-x-point
  (call-with-output-file "proofs/gk_cylindrical_metric_tensor_3d_finite_x_point.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-metric-tensor-3d-finite-x-point geometry-cylindrical `psi
                                               #:nx nx
                                               #:x0 x0
                                               #:x1 x1
                                               #:ny ny
                                               #:y0 y0
                                               #:y1 y1
                                               #:nz nz
                                               #:z0 z0
                                               #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_metric_tensor_3d_finite_x_point.rkt")

;; Show whether 3D metric tensor is finite (excluding the X-point).
(display "Metric tensor finite (excluding X-point): ")
(display proof-gk-cylindrical-metric-tensor-3d-finite-x-point)
(display "\n")

;; Attempt to prove realness of the 3D metric tensor in a cylindrical GK geometry, using automatic differentiation.
(define proof-gk-cylindrical-metric-tensor-3d-real
  (call-with-output-file "proofs/gk_cylindrical_metric_tensor_3d_real.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-metric-tensor-3d-real geometry-cylindrical
                                     #:nx nx
                                     #:x0 x0
                                     #:x1 x1
                                     #:ny ny
                                     #:y0 y0
                                     #:y1 y1
                                     #:nz nz
                                     #:z0 z0
                                     #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_metric_tensor_3d_real.rkt")

;; Show whether 3D metric tensor is real.
(display "Metric tensor real: ")
(display proof-gk-cylindrical-metric-tensor-3d-real)
(display "\n")

;; Attempt to prove realness of the 3D metric tensor in a cylindrical GK geometry (from the X-point outwards), using automatic differentiation.
(define proof-gk-cylindrical-metric-tensor-3d-real-x-point
  (call-with-output-file "proofs/gk_cylindrical_metric_tensor_3d_real_x_point.rkt"
    (lambda (out)
      (parameterize ([current-output-port out] [pretty-print-columns `infinity])
        (display "#lang racket\n\n")
        (display "(require \"../geometry_prover_core.rkt\")\n\n")
        (prove-metric-tensor-3d-real-x-point geometry-cylindrical `psi
                                             #:nx nx
                                             #:x0 x0
                                             #:x1 x1
                                             #:ny ny
                                             #:y0 y0
                                             #:y1 y1
                                             #:nz nz
                                             #:z0 z0
                                             #:z1 z1)))
    #:exists `replace))
(remove-bracketed-expressions-from-file "proofs/gk_cylindrical_metric_tensor_3d_real_x_point.rkt")

;; Show whether 3D metric tensor is real (from the X-point outwards).
(display "Metric tensor real (X-point outwards): ")
(display proof-gk-cylindrical-metric-tensor-3d-real-x-point)
(display "\n\n\n")