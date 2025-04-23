#lang racket

(require "geometry_prover_core.rkt")
(provide convert-expr
         convert-expr-params
         generate-tangent-vectors-3d)

;; Lightweight converter from Racket expressions (expr) into strings representing equivalent C code.
(define (convert-expr expr)
  (match expr
    ;; If expr is of the form "pi", then convert it to "M_PI" in C.
    [`pi "M_PI"]
    
    ;; If expr is a symbol, then convert it directly to a string.
    [(? symbol? symb) (symbol->string symb)]

    ;; If expr is a numerical constant, then convert it directly to a string.
    [(? number? num) (number->string num)]

    ;; If expr is a sum of the form (+ expr1 expr2 ...), then convert it to "(expr1 + expr2 + ...)" in C.
    [`(+ . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " + ") ")"))]
    ;; Likewise for differences.
    [`(- . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " - ") ")"))]

    ;; If expr is a product of the form (* expr1 expr2 ...), then convert it to "(expr1 * expr2 * ...)" in C.
    [`(* . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " * ") ")"))]
    ;; Likewise for quotients.
    [`(/ . ,terms)
     (let ([c-terms (map convert-expr terms)])
       (string-append "(" (string-join c-terms " / ") ")"))]

    ;; If expr is a sine function of the form (sin expr1), then convert it to "sin(expr1)" in C.
    [`(sin ,arg)
     (format "sin(~a)" (convert-expr arg))]

    ;; If expr is a cosine function of the form (cos expr1), then convert it to "cos(expr1)" in C.
    [`(cos ,arg)
     (format "cos(~a)" (convert-expr arg))]

    ;; If expr is a unary function derivative of the form (D (func expr1) var), then convert it to "func_diff_var(expr1)" in C.
    [`(D (,func ,arg) ,var)
     (format "~a_diff_~a(~a)" func (convert-expr var) (convert-expr arg))]

    ;; If expr is a binary function derivative of the form (D (func expr1 expr2) var), then convert it to "func_diff_var(expr1, expr2)" in C.
    [`(D (,func ,arg1 ,arg2) ,var)
     (format "~a_diff_~a(~a, ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a ternary function derivative of the form (D (func expr1 expr2 expr3) var), then convert it to "func_diff_var(expr1, expr2, expr3)" in C.
    [`(D (,func ,arg1 ,arg2 ,arg3) ,var)
     (format "~a_diff_~a(~a, ~a, ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    ;; If expr is a unary function of the form (func expr1), then convert it to "func(expr1)" in C.
    [`(,func ,arg)
     (format "~a(~a)" func (convert-expr arg))]

    ;; If expr is a binary function of the form (func expr1 expr2), then convert it to "func(expr1, expr2)" in C.
    [`(,func ,arg1 ,arg2)
     (format "~a(~a, ~a)" func (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a ternary function of the form (func expr1 expr2 expr3), then convert it to "func(expr1, expr2, expr3)" in C.
    [`(,func ,arg1 ,arg2 ,arg3)
     (format "~a(~a, ~a, ~a)" func (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]))

;; Lightweight converter from Racket function signature expressions (expr) into strings representing equivalent C function signatures.
(define (convert-expr-params expr)
  (match expr
    ;; If expr is a unary function derivative of the form (D (func expr1) var), then convert it to "func_diff_var(double expr1)" in C.
    [`(D (,func ,arg) ,var)
     (format "~a_diff_~a(double ~a)" func (convert-expr var) (convert-expr arg))]

    ;; If expr is a binary function derivative of the form (D (func expr1 expr2) var), then convert it to "func_diff_var(double expr1, double expr2)" in C.
    [`(D (,func ,arg1 ,arg2) ,var)
     (format "~a_diff_~a(double ~a, double ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a ternary function derivative of the form (D (func expr1 expr2 expr3) var), then convert it to "func_diff_var(double expr1, double expr2, double expr3" in C.
    [`(D (,func ,arg1 ,arg2 ,arg3) ,var)
     (format "~a_diff_~a(double ~a, double ~a, double ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    ;; If expr is a unary function of the form (func expr1), then convert it to "func(double expr1)" in C.
    [`(,func ,arg)
     (format "~a(double ~a)" func (convert-expr arg))]

    ;; If expr is a binary function of the form (func expr1 expr2), then convert it to "func(double expr1, double expr2)" in C.
    [`(,func ,arg1 ,arg2)
     (format "~a(double ~a, double ~a)" func (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a ternary function of the form (func expr1 expr2 expr3), then convert it to "func(double expr1, double expr2, double expr3)" in C.
    [`(,func ,arg1 ,arg2 ,arg3)
     (format "~a(double ~a, double ~a, double ~a)" func (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]))

;; -----------------------------------------------
;; 3D Tangent Vector Computation for a GK Geometry
;; -----------------------------------------------
(define (generate-tangent-vectors-3d geometry)
  "Generate C code that computes the 3D tangent vectors for the GK geometry specified by `geometry` using automatic differentiation."
  
  (define name (hash-ref geometry 'name))
  (define exprs (hash-ref geometry 'exprs))
  (define coords (hash-ref geometry 'coords))
  (define func-exprs (hash-ref geometry 'func-exprs))

  (define deriv-exprs (append* (map (lambda (func-expr)
                                      (map (lambda (coord)
                                             `(define ,(symbolic-diff (list-ref func-expr 1) coord)
                                                ,(symbolic-simp (symbolic-diff (list-ref func-expr 2) coord))))
                                           coords))
                                    func-exprs)))
  (define deriv-exprs-filtered (filter (lambda (deriv-expr)
                                         (and (list? (list-ref deriv-expr 1))
                                              (not (null? (list-ref deriv-expr 1)))
                                              (eq? (car (list-ref deriv-expr 1)) `D)))
                                       deriv-exprs))

  (define func-code (cond
                      [(not (empty? func-exprs)) (string-join (map (lambda (func-expr)
                                                                     (string-append "double " (convert-expr-params (list-ref func-expr 1)) "
{
  return " (convert-expr (list-ref func-expr 2)) ";
}"))
                                                                   func-exprs) "\n")]
                      [else ""]))
  (define deriv-code (cond
                       [(not (empty? deriv-exprs-filtered)) (string-join (map (lambda (deriv-expr)
                                                                                (string-append "double " (convert-expr-params (list-ref deriv-expr 1)) "
{
  return " (convert-expr (list-ref deriv-expr 2)) ";
}"))
                                                                              deriv-exprs-filtered) "\n")]
                       [else ""]))

  (define coord-params (string-join (map (lambda (coord)
                                           (string-append "double " (convert-expr coord) ","))
                                         coords)))
  (define coord-params-filtered (substring coord-params 0 (- (string-length coord-params) 1)))

  (define tangent1-exprs (list-ref (symbolic-tangents exprs coords) 0))
  (define tangent2-exprs (list-ref (symbolic-tangents exprs coords) 1))
  (define tangent3-exprs (list-ref (symbolic-tangents exprs coords) 2))
  
  (define code
    (format "
// AUTO-GENERATED CODE FOR GYROKINETIC GEOMETRY: ~a
// Symbolic tangent vector computation for a gyrokinetic geometry in 3D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// Geometry function definitions (if any).
~a

// Geometry derivative definitions (if any).
~a

void compute_tangent_vector1(~a, double tangent_vector1[~a]) {
  tangent_vector1[0] = ~a;
  tangent_vector1[1] = ~a;
  tangent_vector1[2] = ~a;
}

void compute_tangent_vector2(~a, double tangent_vector2[~a]) {
  tangent_vector2[0] = ~a;
  tangent_vector2[1] = ~a;
  tangent_vector2[2] = ~a;
}

void compute_tangent_vector3(~a, double tangent_vector3[~a]) {
  tangent_vector3[0] = ~a;
  tangent_vector3[1] = ~a;
  tangent_vector3[2] = ~a;
}

int main() { }
"
            ;; GK geometry name for code comments.
            name
            ;; Additional geometry functions (e.g. R, phi, Z for cylindrical geometries).
            func-code
            ;; Additional geometry derivative functions (e.g. psi, theta, alpha derivatives of R, phi, Z).
            deriv-code
            ;; Coordinate parameters for function signatures.
            coord-params-filtered
            ;; Number of coordinates.
            (length coords)
            ;; First tangent vector e_1.
            (convert-expr (list-ref tangent1-exprs 0))
            (convert-expr (list-ref tangent1-exprs 1))
            (convert-expr (list-ref tangent1-exprs 2))
            ;; Coordinate parameters for function signatures.
            coord-params-filtered
            ;; Number of coordinates.
            (length coords)
            ;; Second tangent vector e_2.
            (convert-expr (list-ref tangent2-exprs 0))
            (convert-expr (list-ref tangent2-exprs 1))
            (convert-expr (list-ref tangent2-exprs 2))
            ;; Coordinate parameters for function signatures.
            coord-params-filtered
            ;; Number of coordinates.
            (length coords)
            ;; Third tangent vector e_3.
            (convert-expr (list-ref tangent3-exprs 0))
            (convert-expr (list-ref tangent3-exprs 1))
            (convert-expr (list-ref tangent3-exprs 2))))
  code)