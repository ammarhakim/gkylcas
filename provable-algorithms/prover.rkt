#lang racket

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
    [(or (< cfl 0) (> cfl 1)) #f]
    [(or (< nx 1) (>= x0 x1)) #f]
    [(< t-final 0) #f]
    [(not (equal? `(abs ,(symbolic-diff flux-expr cons-expr)) max-speed-expr)) #f]
    [else #t]))