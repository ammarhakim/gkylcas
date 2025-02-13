#lang racket

(require racket/trace)
(provide prove-lax-friedrichs-scalar-1d-stability)

;; Lightweight symbolic differentiator (differentiates expr with respect to var).
(define (symbolic-diff expr var)
  (match expr
    ;; If expr is a symbol, then it either differentiates to 1 (if it's equal to var), or 0 otherwise.
    [(? symbol? symb) (cond
                        [(eq? symb var) 1]
                        [else 0])]

    ;; If expr is a numerical constant, then it differentiates to 0.
    [(? number?) 0]

    ;; If expr is a sum of the form (+ expr1 expr2 ...), then it differentiates to a sum of derivatives (+ expr1' expr2' ...), by linearity.
    [`(+ . ,terms)
     `(+ ,@(map (lambda(term) (symbolic-diff term var)) terms))]

    ;; If expr is a product of the form (* expr1 expr2 ...), then it differentiates to (+ (* expr1' expr2 ...) (* expr1 expr2' ...) ...), by the product rule.
    [`(* . ,terms)
     (define n (length terms))
     (define (mult xs) (cons '* xs)) ; Multiplication helper function.

     ((lambda(sums) (cond
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

    ;; If expr is an absolute value of the form (abs expr1), then it differentiates to (sgn expr1').
    [`(abs ,arg)
     `(* (sgn ,arg) ,(symbolic-diff arg var))]
    
    ;; Otherwise, return false.
    [else #f]))
(trace symbolic-diff)

;; Lightweight symbolic simplification rules (simplifies expr using only correctness-preserving algebraic transformations).
(define (symbolic-simp-rule expr)
  (match expr
    ;; If expr is of the form (0 + x) or (0.0 + x), then simplify to x.
    [`(+ 0 ,x) `,x]
    [`(+ 0.0 ,x) `,x]

    ;; If expr is of the form (1 * x) or (1.0 * x), then simplify to x.
    [`(* 1 ,x) `,x]
    [`(* 1.0 ,x) `,x]

    ;; If expr is of the form (0 * x) or (0.0 * x), then simplify to 0 or 0.0.
    [`(* 0 ,x) 0]
    (`(* 0.0 ,x) 0.0)

    ;; Enforce right associativity of addition: if expr is of the form ((x + y) + z) or (x + y + z), then simplify to (x + (y + z)).
    [`(+ (+ ,x ,y) ,z) `(+ ,x (+ ,y ,z))]
    [`(+ ,x ,y ,z) `(+ (+ ,x ,y) ,z)]

    ;; Enforce right associativity of multiplication: if expr is of the form ((x * y) * z) or (x * y * z), then simplify to (x * (y * z)).
    [`(* (* ,x ,y) ,z) `(* ,x (* ,y ,z))]
    [`(* ,x ,y ,z) `(* (* ,x ,y) ,z)]

    ;; Enforce (reverse) distributive property: if expr is of the form ((a * x) + (b * x)), then simplify to ((a + b) * x).
    [`(+ (* ,a, x) (* ,b ,x)) `(* (+ ,a ,b) ,x)]

    ;; If expr is of the form (x + y) for numeric x and y, then just evaluate the sum.
    [`(+ ,(and x (? number?)) ,(and y (? number?))) (+ x y)]

    ;; If expr is of the form (x * y) for numeric x and y, then just evaluate the product.
    [`(* ,(and x (? number?)) ,(and y (? number?))) (* x y)]

    ;; Move numbers to the left: if expr is of the form (x + y) for non-numeric x but numeric y, then simplify to (y + x).
    [`(+ ,(and x (not (? number?))) ,(and y (? number?))) `(+ ,y ,x)]

    ;; Move numbers to the left: if expr is of the form (x * y) for non-numeric x but numeric y, then simplify to (y * x).
    [`(* ,(and x (not (? number?))) ,(and y (? number?))) `(* ,y ,x)]

    ;; If expr is a sum of the form (x + y + ...), then apply symbolic simplification to each term x, y, ... in the sum.
    [`(+ . ,terms)
     `(+ ,@(map (lambda(term) (symbolic-simp-rule term)) terms))]

    ;; If expr is a product of the form (x * y * ...), then apply symbolic simplification to each term x, y, ... in the product.
    [`(* . ,terms)
     `(* ,@(map (lambda(term) (symbolic-simp-rule term)) terms))]

    ;; Otherwise, return the expression.
    [else expr]))
(trace symbolic-simp-rule)

;; Recursively apply the symbolic simplification rules until the expression stops changing (fixed point).
(define (symbolic-simp expr)
  (cond
    [(equal? (symbolic-simp-rule expr) expr) expr]
    [else (symbolic-simp (symbolic-simp-rule expr))]))
(trace symbolic-simp)

;; -------------------------------------------------------------------------------------------------
;; Prove L-1/L-2/L-infinity stability of Lax–Friedrichs (Finite-Difference) Solver for 1D Scalar PDE
;; -------------------------------------------------------------------------------------------------
(define (prove-lax-friedrichs-scalar-1d-stability pde
                                                     #:nx [nx 200]
                                                     #:x0 [x0 0.0]
                                                     #:x1 [x1 2.0]
                                                     #:t-final [t-final 1.0]
                                                     #:cfl [cfl 0.95]
                                                     #:init-func
                                                     [init-func "(x < 1.0) ? 1.0 : 0.0"])
   "Prove that the Lax-Friedrichs finite-difference method is L-1/L-2/L-infinity stable for the 1D scalar PDE specified by `pde`. 
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: C code for the initial condition, e.g. piecewise constant."

  (define cons-expr (hash-ref pde 'cons-expr))
  (define flux-expr (hash-ref pde 'flux-expr))
  (define max-speed-expr (hash-ref pde 'max-speed-expr))

  (cond
    ;; Check whether the CFL coefficient is greater than 0 and less than or equal to 1 (otherwise, return false).
    [(or (<= cfl 0) (> cfl 1)) #f]
    
    ;; Check whether the number of spatial cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false)
    [(or (< nx 1) (>= x0 x1)) #f]
    
    ;; Check whether the final simulation time is non-negative (otherwise, return false).
    [(< t-final 0) #f]
    
    ;; Check whether the absolute value of the derivative of the flux derivative is symbolically equivalent to the maximum wave-speed estimate (otherwise, return false).
    [(not (equal? `(abs ,(symbolic-simp (symbolic-diff flux-expr cons-expr))) (symbolic-simp max-speed-expr))) #f]

    ;; Otherwise, return true.
    [else #t]))