#lang racket

(require racket/trace)
(current-prefix-in " ")
(current-prefix-out " ")

(provide symbolic-diff
         symbolic-simp-rule
         symbolic-simp
         symbolic-tangents
         is-finite)

;; Lightweight symbolic differentiator (differentiates expr with respect to var).
(define (symbolic-diff expr var)
  (match expr
    ;; If expr is a symbol, then it either differentiates to 1 (if it's equal to var), or 0 otherwise.
    [(? symbol? symb) (cond
                        [(eq? symb var) 1.0]
                        [else 0.0])]

    ;; If expr is a numerical constant, then it differentiates to 0.
    [(? number?) 0.0]

    ;; If expr is a sum of the form (+ expr1 expr2 ...), then it differentiates to a sum of derivatives (+ expr1' expr2' ...), by linearity.
    [`(+ . ,terms)
     `(+ ,@(map (lambda (term) (symbolic-diff term var)) terms))]
    ;; Likewise for differences of the form (- expr1 expr2 ...), which differentiate to (- expr1' expr2' ...), by linearity.
    [`(- . ,terms)
     `(- ,@(map (lambda (term) (symbolic-diff term var)) terms))]

    ;; If expr is a product of the form (* expr1 expr2 ...), then it differentiates to (+ (* expr1' expr2 ...) (* expr1 expr2' ...) ...), by the product rule.
    [`(* . ,terms)
     (define n (length terms))
     (define (mult xs) (cons '* xs)) ; Multiplication helper function.

     ((lambda (sums) (cond
                      [(null? (cdr sums)) (car sums)]
                      [else (cons '+ sums)]))
      (let loop ([i 0])
        (cond
          [(= i n) `()]
          [else
           ;; Evaluate the derivative of the i-th term in the product.
           (let ([di (symbolic-diff (list-ref terms i) var)])
             (cons
              (mult (for/list ([j (in-range n)])
                      (cond
                        [(= j i) di]
                        [else (list-ref terms j)])))
              (loop (add1 i))))])))]

    ;; If expr is a sine function of the form (sin expr1), then it differentiates to (* (cos expr1) expr1').
    [`(sin ,arg)
     `(* (cos ,arg) ,(symbolic-diff arg var))]

    ;; If expr is a cosine function of the form (cos expr1), then it differentiates to (* (* -1.0 (sin expr1)) expr1').
    [`(cos ,arg)
     `(* (* -1.0 (sin ,arg)) ,(symbolic-diff arg var))]

    ;; If expr is a unary function of the form (func expr1), then it differentiates to (func' expr1) if expr1 is equal to var, or 0 otherwise.
    [`(,func ,arg) (cond
                     [(eq? arg var) `(D (,func ,arg) ,var)]
                     [else 0.0])]

    ;; If expr is a binary function of the form (func expr1 expr2), then it differentiates to (func' expr1 expr2) if expr1 or expr2 is equal to var,
    ;; and 0 otherwise.
    [`(,func ,arg1 ,arg2) (cond
                            [(or (eq? arg1 var) (eq? arg2 var)) `(D (,func ,arg1 ,arg2) ,var)]
                            [else 0.0])]

    ;; If expr is a ternary function of the form (func expr1 expr2 expr3), then it differentiates to (func' expr1 expr2 expr3) if expr1 or expr2 or expr3
    ;; is equal to var, and 0 otherwise.
    [`(,func ,arg1 ,arg2 ,arg3) (cond
                                  [(or (eq? arg1 var) (eq? arg2 var) (eq? arg3 var)) `(D (,func ,arg1 ,arg2 ,arg3) ,var)]
                                  [else 0.0])]

    ;; Otherwise, return false.
    [else #f]))

;; Lightweight symbolic simplification rules (simplifies expr using only correctness-preserving algebraic transformations).
(define (symbolic-simp-rule expr)
  (match expr    
    ;; If expr is of the form (0 + x) or (0.0 + x), then simplify to x.
    [`(+ 0 ,x) `,x]
    [`(+ 0.0 ,x) `,x]
    [`(+ -0.0 ,x) `,x]

    ;; If expr is of the form (1 * x) or (1.0 * x), then simplify to x.
    [`(* 1 ,x) `,x]
    [`(* 1.0 ,x) `,x]

    ;; If expr is of the form (0 * x) or (0.0 * x), then simplify to 0 or 0.0.
    [`(* 0 ,x) 0]
    [`(* 0.0 ,x) 0.0]
    [`(* -0.0 ,x) 0.0]

    ;; If expr is of the form (x - 0) or (x - 0.0), then simplify to x.
    [`(- ,x 0) `,x]
    [`(- ,x 0.0) `,x]
    [`(- ,x -0.0) `,x]

    ;; If expr is of the form (0 - x) or (0.0 - x), then simplify to (-1 * x) or (-1.0 * x).
    [`(- 0 ,x) `(* -1 ,x)]
    [`(- 0.0 ,x) `(* -1.0 ,x)]
    [`(- -0.0 ,x) `(* -1.0 ,x)]

    ;; Enforce right associativity of addition: if expr is of the form ((x + y) + z) or (x + y + z), then simplify to (x + (y + z)).
    [`(+ (+ ,x ,y) ,z) `(+ ,x (+ ,y ,z))]
    [`(+ ,x ,y ,z) `(+ (+ ,x ,y) ,z)]

    ;; Enforce right associativity of multiplication: if expr is of the form ((x * y) * z) or (x * y * z), then simplify to (x * (y * z)).
    [`(* (* ,x ,y) ,z) `(* ,x (* ,y ,z))]
    [`(* ,x ,y ,z) `(* (* ,x ,y) ,z)]

    ;; Move numbers to the left: if expr is of the form (x + y) for non-numeric x but numeric y, then simplify to (y + x).
    [`(+ ,(and x (not (? number?))) ,(and y (? number?))) `(+ ,y ,x)]

    ;; Move numbers to the left: if expr is of the form (x * y) for non-numeric x but numeric y, then simplify to (y * x).
    [`(* ,(and x (not (? number?))) ,(and y (? number?))) `(* ,y ,x)]

    ;; If expr is of the form (x * (y * z)) for numeric y and non-numeric x and z, then simplify to (y * (x * z)).
    [`(* ,(and x (not (? number?))) (* ,(and y (? number?)) ,(and z (not (? number?))))) `(* ,y (* ,x ,z))]

    [`(+ ,x (* -1 ,y)) `(- ,x ,y)]
    [`(+ ,x (* -1.0 ,y)) `(- ,x ,y)]

    ;; If expr is a sum of the form (x + y + ...), then apply symbolic simplification to each term x, y, ... in the sum.
    [`(+ . ,terms)
     `(+ ,@(map (lambda (term) (symbolic-simp-rule term)) terms))]
    ;; Likewise for differences.
    [`(- . ,terms)
     `(- ,@(map (lambda (term) (symbolic-simp-rule term)) terms))]

    ;; If expr is a product of the form (x * y * ...), then apply symbolic simplification to each term x, y, ... in the product.
    [`(* . ,terms)
     `(* ,@(map (lambda (term) (symbolic-simp-rule term)) terms))]
    ;; Likewise for quotients.
    [`(/ . ,terms)
     `(/ ,@(map (lambda (term) (symbolic-simp-rule term)) terms))]

    ;; Otherwise, return the expression.
    [else expr]))

;; Recursively apply the symbolic simplification rules until the expression stops changing (fixed point).
(define (symbolic-simp expr)
  (define simp-expr (symbolic-simp-rule expr))
  
  (cond
    [(equal? simp-expr expr) expr]
    [else (symbolic-simp simp-expr)]))

;; Compute symbolic tangent vectors by applying symbolic differentiation to exprs, mapped over vars.
(define (symbolic-tangents exprs coords)
  (map (lambda (coord)
         (map (lambda (expr)
                (symbolic-simp (symbolic-diff expr coord)))
              exprs))
       coords))

;; Recursively determine whether an expression is finite.
(define (is-finite expr finite-exprs)
  (match expr
    ;; A finite expression is, trivially, finite.
    [(? (lambda (arg)
          (and (not (empty? finite-exprs)) (ormap (lambda (finite-expr)
                                                    (equal? arg finite-expr)))))) #t]

    ;; The sum of two finite expressions is always finite.
    [`(+ ,x ,y) (and (is-finite x finite-exprs) (is-finite y finite-exprs))]

    ;; The product of two finite expressions is always finite.
    [`(* ,x ,y) (and (is-finite x finite-exprs) (is-finite y finite-exprs))]

    ;; Otherwise, assume false.
    [else #f]))