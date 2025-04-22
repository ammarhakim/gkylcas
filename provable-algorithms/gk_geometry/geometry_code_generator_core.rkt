#lang racket

(require "geometry_prover_core.rkt")
(provide convert-expr
         convert-expr-params
         generate-tangent-vectors-3d)

;; Lightweight converter from Racket expressions (expr) into strings representing equivalent C code.
(define (convert-expr expr)
  (match expr
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

    [`(D (,func ,arg) ,var)
     (format "~a_diff_~a(~a)" func (convert-expr var) (convert-expr arg))]

    [`(D (,func ,arg1 ,arg2) ,var)
     (format "~a_diff_~a(~a, ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2))]

    [`(D (,func ,arg1 ,arg2 ,arg3) ,var)
     (format "~a_diff_~a(~a, ~a, ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    [`(,func ,arg)
     (format "~a(~a)" func (convert-expr arg))]

    [`(,func ,arg1 ,arg2)
     (format "~a(~a, ~a)" func (convert-expr arg1) (convert-expr arg2))]

    [`(,func ,arg1 ,arg2 ,arg3)
     (format "~a(~a, ~a, ~a)" func (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]))

(define (convert-expr-params expr)
  (match expr
    [`(D (,func ,arg) ,var)
     (format "~a_diff_~a(double ~a)" func (convert-expr var) (convert-expr arg))]

    [`(D (,func ,arg1 ,arg2) ,var)
     (format "~a_diff_~a(double ~a, double ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2))]

    [`(D (,func ,arg1 ,arg2 ,arg3) ,var)
     (format "~a_diff_~a(double ~a, double ~a, double ~a)" func (convert-expr var) (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]
    
    [`(,func ,arg)
     (format "~a(double ~a)" func (convert-expr arg))]

    [`(,func ,arg1 ,arg2)
     (format "~a(double ~a, double ~a)" func (convert-expr arg1) (convert-expr arg2))]

    [`(,func ,arg1 ,arg2 ,arg3)
     (format "~a(double ~a, double ~a, double ~a)" func (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]))

(define (generate-tangent-vectors-3d geometry)
  (define name (hash-ref geometry 'name))
  (define exprs (hash-ref geometry 'exprs))
  (define coords (hash-ref geometry 'coords))
  (define func-exprs (hash-ref geometry 'func-exprs))

  (define deriv-exprs (append* (map (lambda (func-expr)
                                      (map (lambda (coord)
                                             (symbolic-diff func-expr coord))
                                           coords))
                                    func-exprs)))
  (define deriv-exprs-filtered (filter (lambda (deriv-expr)
                                         (and (list? deriv-expr)
                                              (not (null? deriv-expr))
                                              (eq? (car deriv-expr) `D)))
                                       deriv-exprs))

  (define func-code (cond
                      [(not (empty? func-exprs)) (string-join (map (lambda (func-expr)
                                                                     (string-append "double " (convert-expr-params func-expr) " { return 0.0; }"))
                                                                   func-exprs) "\n")]
                      [else ""]))
  (define deriv-code (cond
                       [(not (empty? deriv-exprs-filtered)) (string-join (map (lambda (deriv-expr)
                                                                                (string-append "double " (convert-expr-params deriv-expr) " { return 0.0; }"))
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

// Geometry function placeholders (if any).
~a

// Geometry derivative placeholders (if any).
~a

double* compute_tangent_vector1(~a) {
  double *tangent_vector1 = (double*) malloc(~a * sizeof(double));

  tangent_vector1[0] = ~a;
  tangent_vector1[1] = ~a;
  tangent_vector1[2] = ~a;

  return tangent_vector1;
}

double* compute_tangent_vector2(~a) {
  double *tangent_vector2 = (double*) malloc(~a * sizeof(double));

  tangent_vector2[0] = ~a;
  tangent_vector2[1] = ~a;
  tangent_vector2[2] = ~a;

  return tangent_vector2;
}

double* compute_tangent_vector3(~a) {
  double *tangent_vector3 = (double*) malloc(~a * sizeof(double));

  tangent_vector3[0] = ~a;
  tangent_vector3[1] = ~a;
  tangent_vector3[2] = ~a;

  return tangent_vector3;
}

int main() { }
"
            name
            func-code
            deriv-code
            coord-params-filtered
            (length coords)
            (convert-expr (list-ref tangent1-exprs 0))
            (convert-expr (list-ref tangent1-exprs 1))
            (convert-expr (list-ref tangent1-exprs 2))
            coord-params-filtered
            (length coords)
            (convert-expr (list-ref tangent2-exprs 0))
            (convert-expr (list-ref tangent2-exprs 1))
            (convert-expr (list-ref tangent2-exprs 2))
            coord-params-filtered
            (length coords)
            (convert-expr (list-ref tangent3-exprs 0))
            (convert-expr (list-ref tangent3-exprs 1))
            (convert-expr (list-ref tangent3-exprs 2))))
  code)