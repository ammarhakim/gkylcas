#lang racket

(require racket/trace)
(current-prefix-in " ")
(current-prefix-out " ")

(provide symbolic-diff
         symbolic-simp-rule
         symbolic-simp
         symbolic-tangents
         is-non-negative
         is-real
         is-real-non-negative
         is-non-zero
         is-finite
         is-finite-non-zero
         variable-transform
         prove-tangent-vectors-3d-finite
         prove-tangent-vectors-3d-finite-x-point
         prove-tangent-vectors-3d-real
         prove-tangent-vectors-3d-real-x-point)

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

    ;; If expr is a quotient of the form (/ expr1 expr2), then it differentiates to (/ (- (* expr2 expr1') (expr1 expr2') (* expr2 expr2)), by the quotient rule.
    [`(/ ,x ,y)
     `(/ (- (* ,y ,(symbolic-diff x var)) (* ,x ,(symbolic-diff y var))) (* ,y ,y))]

    ;; If expr is a square root of the form (sqrt expr1), then it differentiates to (/ expr1 (* 2.0 (sqrt expr1))).
    [`(sqrt ,arg)
     `(/ ,(symbolic-diff arg var) (* 2.0 (sqrt ,arg)))]

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

    ;; If expr is of the form (x + 0) or (x + 0.0), then simplify to x.
    [`(+ ,x 0) `,x]
    [`(+ ,x 0.0) `,x]
    [`(+ ,x -0.0) `,x]

    ;; If expr is of the form (1 * x) or (1.0 * x), then simplify to x.
    [`(* 1 ,x) `,x]
    [`(* 1.0 ,x) `,x]

    ;; If expr is of the form (x * 1) or (x * 1.0), then simplify to x.
    [`(* ,x 1) `,x]
    [`(* ,x 1.0) `,x]

    ;; If expr is of the form (0 * x) or (0.0 * x), then simplify to 0 or 0.0.
    [`(* 0 ,x) 0]
    [`(* 0.0 ,x) 0.0]
    [`(* -0.0 ,x) 0.0]

    ;; If expr is of the form (x * 0) or (x * 0.0), then simplify to 0 or 0.0.
    [`(* ,x 0) 0]
    [`(* ,x 0.0) 0.0]
    [`(* ,x -0.0) 0.0]

    ;; If expr is of the form (x - 0) or (x - 0.0), then simplify to x.
    [`(- ,x 0) `,x]
    [`(- ,x 0.0) `,x]
    [`(- ,x -0.0) `,x]

    ;; If expr is of the form (0 - x) or (0.0 - x), then simplify to (-1 * x) or (-1.0 * x).
    [`(- 0 ,x) `(* -1 ,x)]
    [`(- 0.0 ,x) `(* -1.0 ,x)]
    [`(- -0.0 ,x) `(* -1.0 ,x)]

    ;; If expr is of the form (0 / x) or (0.0 / x), then simplify to 0 or 0.0.
    [`(/ 0 ,x) 0]
    [`(/ 0.0 ,x) 0.0]
    [`(/ -0.0 ,x) 0.0]

    ;; If expr is of the form (x / x), then simplify to 1.0
    [`(/ ,x ,x) 1.0]

    ;; If expr is of the form (x / (x * y)) or (x / (y * x)), then simplify to (1.0 / y).
    [`(/ ,x (* ,x ,y)) `(/ 1.0 ,y)]
    [`(/ ,x (* ,y ,x)) `(/ 1.0 ,y)]

    ;; Enforce right associativity of addition: if expr is of the form ((x + y) + z) or (x + y + z), then simplify to (x + (y + z)).
    [`(+ (+ ,x ,y) ,z) `(+ ,x (+ ,y ,z))]
    [`(+ ,x ,y ,z) `(+ (+ ,x ,y) ,z)]

    ;; Enforce right associativity of multiplication: if expr is of the form ((x * y) * z) or (x * y * z), then simplify to (x * (y * z)).
    [`(* (* ,x ,y) ,z) `(* ,x (* ,y ,z))]
    [`(* ,x ,y ,z) `(* (* ,x ,y) ,z)]

    ;; If expr is of the form (x + y) for numeric x and y, then just evaluate the sum. Likewise for differences.
    [`(+ ,(and x (? number?)) ,(and y (? number?))) (+ x y)]
    [`(- ,(and x (? number?)) ,(and y (? number?))) (- x y)]

    ;; If expr is of the form (x * y) for numeric x and y, then just evaluate the product. Likewise for quotients
    [`(* ,(and x (? number?)) ,(and y (? number?))) (* x y)]
    [`(/ ,(and x (? number?)) ,(and y (? number?))) (/ x y)]

    ;; If expr is of the form (x * (y * z)) for numeric numeric x and y, then evaluate the product of x and y.
    [`(* ,(and x (? number?)) (* ,(and y (? number?)) ,z)) `(* ,(* x y) ,z)]

    ;; Move numbers to the left: if expr is of the form (x + y) for non-numeric x but numeric y, then simplify to (y + x).
    [`(+ ,(and x (not (? number?))) ,(and y (? number?))) `(+ ,y ,x)]

    ;; Move numbers to the left: if expr is of the form (x * y) for non-numeric x but numeric y, then simplify to (y * x).
    [`(* ,(and x (not (? number?))) ,(and y (? number?))) `(* ,y ,x)]

    ;; If expr is of the form (x * (y * z)) for numeric y and non-numeric x and z, then simplify to (y * (x * z)).
    [`(* ,(and x (not (? number?))) (* ,(and y (? number?)) ,(and z (not (? number?))))) `(* ,y (* ,x ,z))]

    ;; If expr is of the form (x + (-1 * y)) or (x + (-1.0 * y)), then simplify to (x - y).
    [`(+ ,x (* -1 ,y)) `(- ,x ,y)]
    [`(+ ,x (* -1.0 ,y)) `(- ,x ,y)]

    ;; If expr is of the form sqrt(x * y), then simplify to (sqrt(x) * sqrt(y)).
    [`(sqrt (* ,x ,y)) `(* (sqrt ,x) (sqrt ,y))]

    ;; If expr if of the form sqrt(x) for numeric x, then just evaluate the square root.
    [`(sqrt ,(and x (? number?))) (sqrt x)]

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

;; Determine whether an expression is non-negative.
(define (is-non-negative expr non-negative)
  (match expr
    ;; A non-negative number is, trivially, non-negative.
    [(? (lambda (arg)
          (and (number? arg) (or (>= arg 0) (>= arg 0.0))))) #t]

    ;; Non-negative variables are, trivially, non-zero.
    [(? (lambda (arg)
          (and (not (empty? non-negative)) (ormap (lambda (non-negative-var)
                                                    (equal? arg non-negative-var)) non-negative)))) #t]

    ;; The sum, product, or quotient of two non-negative numbers is always non-negative.
    [`(+ ,x ,y) (and (is-non-negative x non-negative) (is-non-negative y non-negative))]
    [`(* ,x ,y) (and (is-non-negative x non-negative) (is-non-negative y non-negative))]
    [`(/ ,x ,y) (and (is-non-negative x non-negative) (is-non-negative y non-negative))]

    ;; Otherwise, assume false.
    [else #f]))

;; Recursively determine whether an expression corresponds to a real number.
(define (is-real expr func-exprs deriv-exprs coords)
  (match expr
    ;; Real numbers are trivially real.
    [(? real?) #t]

    ;; Pi (the constant) is real.
    [`pi #t]

    ;; Geometry functions whose definitions are real are, trivially, real.
    [(? (lambda (arg)
          (and (not (empty? func-exprs)) (ormap (lambda (func-expr)
                                                  (and (equal? arg (list-ref func-expr 1)) (is-real (list-ref func-expr 2) func-exprs deriv-exprs coords)))
                                                func-exprs)))) #t]

    ;; Geometry function derivatives whose definitions are real are, trivially, real.
    [(? (lambda (arg)
          (and (not (empty? deriv-exprs)) (ormap (lambda (deriv-expr)
                                                   (and (equal? arg (list-ref deriv-expr 1)) (is-real (list-ref deriv-expr 2) func-exprs deriv-exprs coords)))
                                                 deriv-exprs)))) #t]

    ;; Coordinates are assumed to be real (this is enforced elsewhere).
    [(? (lambda (arg)
          (and (not (empty? coords)) (ormap (lambda (coord)
                                              (equal? arg coord)) coords)))) #t]

    ;; The square root of a non-negative expression is always real.
    [`(sqrt ,x) (is-non-negative x `())]

    ;; The sine or cosine of a real expression is always real.
    [`(sin ,x) (is-real x func-exprs deriv-exprs coords)]
    [`(cos ,x) (is-real x func-exprs deriv-exprs coords)]

    ;; The sum, difference, product, or quotient of two real numbers is always real.
    [`(+ . ,terms)
     (andmap (lambda (term) (is-real term func-exprs deriv-exprs coords)) terms)]
    [`(- . ,terms)
     (andmap (lambda (term) (is-real term func-exprs deriv-exprs coords)) terms)]
    [`(* . ,terms)
     (andmap (lambda (term) (is-real term func-exprs deriv-exprs coords)) terms)]
    [`(/ . ,terms)
     (andmap (lambda (term) (is-real term func-exprs deriv-exprs coords)) terms)]
    
    ;; Otherwise, assume false.
    [else #f]))

;; Recursively determine whether an expression corresponds to a real number, assuming that certain quantities are strictly non-negative.
(define (is-real-non-negative expr non-negative func-exprs deriv-exprs coords)
  (match expr
    ;; Real numbers are trivially real.
    [(? real?) #t]

    ;; Pi (the constant) is real.
    [`pi #t]

    ;; Geometry functions whose definitions are real are, trivially, real.
    [(? (lambda (arg)
          (and (not (empty? func-exprs)) (ormap (lambda (func-expr)
                                                  (and (equal? arg (list-ref func-expr 1)) (is-real-non-negative (list-ref func-expr 2) non-negative func-exprs deriv-exprs coords)))
                                                func-exprs)))) #t]

    ;; Geometry function derivatives whose definitions are real are, trivially, real.
    [(? (lambda (arg)
          (and (not (empty? deriv-exprs)) (ormap (lambda (deriv-expr)
                                                   (and (equal? arg (list-ref deriv-expr 1)) (is-real-non-negative (list-ref deriv-expr 2) non-negative func-exprs deriv-exprs coords)))
                                                 deriv-exprs)))) #t]

    ;; Coordinates are assumed to be real (this is enforced elsewhere).
    [(? (lambda (arg)
          (and (not (empty? coords)) (ormap (lambda (coord)
                                              (equal? arg coord)) coords)))) #t]

    ;; The square root of a non-negative expression is always real.
    [`(sqrt ,x) (is-non-negative x non-negative)]

    ;; The sine or cosine of a real expression is always real.
    [`(sin ,x) (is-real-non-negative x non-negative func-exprs deriv-exprs coords)]
    [`(cos ,x) (is-real-non-negative x non-negative func-exprs deriv-exprs coords)]

    ;; The sum, difference, product, or quotient of two real numbers is always real.
    [`(+ . ,terms)
     (andmap (lambda (term) (is-real-non-negative term non-negative func-exprs deriv-exprs coords)) terms)]
    [`(- . ,terms)
     (andmap (lambda (term) (is-real-non-negative term non-negative func-exprs deriv-exprs coords)) terms)]
    [`(* . ,terms)
     (andmap (lambda (term) (is-real-non-negative term non-negative func-exprs deriv-exprs coords)) terms)]
    [`(/ . ,terms)
     (andmap (lambda (term) (is-real-non-negative term non-negative func-exprs deriv-exprs coords)) terms)]
    
    ;; Otherwise, assume false.
    [else #f]))

;; Determine whether an expression is non-zero.
(define (is-non-zero expr non-zero)
  (match expr
    ;; A non-zero number is, trivially, non-zero.
    [(? (lambda (arg)
         (and (number? arg) (not (equal? arg 0)) (not (equal? arg 0.0))))) #t]

    ;; Pi (the constant) is non-zero.
    [`pi #t]

    ;; Non-zero variables are, trivially, non-zero.
    [(? (lambda (arg)
          (and (not (empty? non-zero)) (ormap (lambda (non-zero-var)
                                                (equal? arg non-zero-var)) non-zero)))) #t]

    ;; The product of two non-zero numbers is always non-zero.
    [`(* ,x ,y) (and (is-non-zero x non-zero) (is-non-zero y non-zero))]

    ;; The square root of a non-zero number is always non-zero.
    [`(sqrt ,x) (is-non-zero x non-zero)]
    
    ;; Otherwise, assume false.
    [else #f]))

;; Recursively determine whether an expression is finite.
(define (is-finite expr func-exprs deriv-exprs coords)
  (match expr
    ;; A non-infinite number is, trivially, finite.
    [(? (lambda (arg)
          (and (number? arg) (not (equal? arg +inf.0)) (not (equal? arg -inf.0))))) #t]

    ;; Geometry functions whose definitions are finite are, trivially, finite.
    [(? (lambda (arg)
          (and (not (empty? func-exprs)) (ormap (lambda (func-expr)
                                                  (and (equal? arg (list-ref func-expr 1)) (is-finite (list-ref func-expr 2) func-exprs deriv-exprs coords)))
                                                func-exprs)))) #t]
    
    ;; Geometry function derivatives whose definitions are finite are, trivially, finite.
    [(? (lambda (arg)
          (and (not (empty? deriv-exprs)) (ormap (lambda (deriv-expr)
                                                   (and (equal? arg (list-ref deriv-expr 1)) (is-finite (list-ref deriv-expr 2) func-exprs deriv-exprs coords)))
                                                 deriv-exprs)))) #t]

    ;; Coordinates are assumed to be finite (this is enforced elsewhere).
    [(? (lambda (arg)
          (and (not (empty? coords)) (ormap (lambda (coord)
                                              (equal? arg coord)) coords)))) #t]

    ;; The sine or cosine of a real expression is always finite.
    [`(sin ,x) (is-real x func-exprs deriv-exprs coords)]
    [`(cos ,x) (is-real x func-exprs deriv-exprs coords)]

    ;; The square root of a finite expression is always finite.
    [`(sqrt ,x) (is-finite x func-exprs deriv-exprs coords)]

    ;; The sum or difference of two finite expressions is always finite.
    [`(+ ,x ,y) (and (is-finite x func-exprs deriv-exprs coords) (is-finite y func-exprs deriv-exprs coords))]
    [`(- ,x ,y) (and (is-finite x func-exprs deriv-exprs coords) (is-finite y func-exprs deriv-exprs coords))]

    ;; The product of two finite expressions is always finite.
    [`(* ,x ,y) (and (is-finite x func-exprs deriv-exprs coords) (is-finite y func-exprs deriv-exprs coords))]

    ;; The quotient of a finite expression by a non-zero expression is always finite.
    [`(/ ,x ,y) (and (is-finite x func-exprs deriv-exprs coords) (is-non-zero y `()))]

    ;; Otherwise, assume false.
    [else #f]))

;; Recursively determine whether an expression is finite, assuming that certain quantities are strictly non-zero.
(define (is-finite-non-zero expr non-zero func-exprs deriv-exprs coords)
  (match expr
    ;; A non-infinite number is, trivially, finite.
    [(? (lambda (arg)
          (and (number? arg) (not (equal? arg +inf.0)) (not (equal? arg -inf.0))))) #t]

    ;; Geometry functions whose definitions are finite are, trivially, finite.
    [(? (lambda (arg)
          (and (not (empty? func-exprs)) (ormap (lambda (func-expr)
                                                  (and (equal? arg (list-ref func-expr 1)) (is-finite-non-zero (list-ref func-expr 2) non-zero func-exprs deriv-exprs coords)))
                                                func-exprs)))) #t]

    ;; Geometry function derivatives whose definitions are finite are, trivially, finite.
    [(? (lambda (arg)
          (and (not (empty? deriv-exprs)) (ormap (lambda (deriv-expr)
                                                   (and (equal? arg (list-ref deriv-expr 1)) (is-finite-non-zero (list-ref deriv-expr 2) non-zero func-exprs deriv-exprs coords)))
                                                 deriv-exprs)))) #t]

    ;; Coordinates are assumed to be finite (this is enforced elsewhere).
    [(? (lambda (arg)
          (and (not (empty? coords)) (ormap (lambda (coord)
                                              (equal? arg coord)) coords)))) #t]

    ;; The sine or cosine of a real expression is always finite.
    [`(sin ,x) (is-real x func-exprs deriv-exprs coords)]
    [`(cos ,x) (is-real x func-exprs deriv-exprs coords)]

    ;; The square root of a finite expression is always finite.
    [`(sqrt ,x) (is-finite-non-zero x non-zero func-exprs deriv-exprs coords)]

    ;; The sum or difference of two finite expressions is always finite.
    [`(+ ,x ,y) (and (is-finite-non-zero x non-zero func-exprs deriv-exprs coords) (is-finite-non-zero y non-zero func-exprs deriv-exprs coords))]
    [`(- ,x ,y) (and (is-finite-non-zero x non-zero func-exprs deriv-exprs coords) (is-finite-non-zero y non-zero func-exprs deriv-exprs coords))]

    ;; The product of two finite expressions is always finite.
    [`(* ,x ,y) (and (is-finite-non-zero x non-zero func-exprs deriv-exprs coords) (is-finite-non-zero y non-zero func-exprs deriv-exprs coords))]

    ;;The quotient of a finite expression by a non-zero expression is always finite.
    [`(/ ,x ,y) (and (is-finite-non-zero x non-zero func-exprs deriv-exprs coords) (is-non-zero y non-zero))]

    ;; Otherwise, assume false.
    [else #f]))

;; Recursively transform all occurrences of a given variable within an expression to a new variable.
(define (variable-transform expr var new-var)
  (cond
    ;; Replace any occurrence of var in expr with new-var.
    [(symbol? expr) (cond
                      [(equal? expr var) new-var]
                      [else expr])]

    ;; Recursively apply variable-transform to all subexpressions.
    [(pair? expr) (map (lambda (subexpr)
                         (variable-transform subexpr var new-var)) expr)]

    ;; Otherwise, return the expression.
    [else expr]))

;; ---------------------------------------------------------------------------------------------
;; Prove Finiteness of the 3D Tangent Vectors for a GK Geometry, using Automatic Differentiation
;; ---------------------------------------------------------------------------------------------
(define (prove-tangent-vectors-3d-finite geometry
                                         #:nx [nx 100]
                                         #:x0 [x0 0.0]
                                         #:x1 [x1 1.0]
                                         #:ny [ny 100]
                                         #:y0 [y0 0.0]
                                         #:y1 [y1 1.0]
                                         #:nz [nz 100]
                                         #:z0 [z0 0.0]
                                         #:z1 [z1 1.0])
  "Prove that the 3D tangent vectors remain finite everywhere for the GK geometry specified by `geometry` using automatic differentiation.
  - `nx`: Number of cells in the x-direction.
  - `x0`, `x1`: Domain boundaries in the x-direction.
  - `ny`: Number of cells in the y-direction.
  - `y0`, `y1`: Domain boundaries in the y-direction.
  - `nz`: Number of cells in the z-direction.
  - `z0`, `z1`: Domain boundaries in the z-direction."

  (define exprs (hash-ref geometry 'exprs))
  (define coords (hash-ref geometry 'coords))
  (define func-exprs (hash-ref geometry 'func-exprs))

  (trace symbolic-diff)
  (trace symbolic-simp-rule)
  (trace symbolic-simp)
  (trace symbolic-tangents)
  (trace is-non-negative)
  (trace is-real)
  (trace is-non-zero)
  (trace is-finite)

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

  (define tangent-vectors (symbolic-tangents exprs coords))
  (define tangent1-exprs (list-ref tangent-vectors 0))
  (define tangent2-exprs (list-ref tangent-vectors 1))
  (define tangent3-exprs (list-ref tangent-vectors 2))

  (define out (cond
                ;; Check whether the number of x-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nx 1) (>= x0 x1)) #f]

                ;; Check whether the number of y-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< ny 1) (>= y0 y1)) #f]

                ;; Check whether the number of z-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nz 1) (>= z0 z1)) #f]

                ;; Check whether the domain boundaries, any hence the coordinates, correspond to real numbers (otherwise, return false).
                [(or (not (is-real x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the domain boundaries, and hence the coordinates, are finite (otherwise, return false).
                [(or (not (is-finite x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the components of the first tangent vector e_1 are all finite (otherwise, return false).
                [(or (not (is-finite (list-ref tangent1-exprs 0) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite (list-ref tangent1-exprs 1) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite (list-ref tangent1-exprs 2) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the second tangent vector e_2 are all finite (otherwise, return false).
                [(or (not (is-finite (list-ref tangent2-exprs 0) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite (list-ref tangent2-exprs 1) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite (list-ref tangent2-exprs 2) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the third tangent vector e_3 are all finite (otherwise, return false).
                [(or (not (is-finite (list-ref tangent3-exprs 0) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite (list-ref tangent3-exprs 1) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite (list-ref tangent3-exprs 2) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Otherwise, return true.
                [else #t]))

  (untrace symbolic-diff)
  (untrace symbolic-simp-rule)
  (untrace symbolic-simp)
  (untrace symbolic-tangents)
  (untrace is-non-negative)
  (untrace is-real)
  (untrace is-non-zero)
  (untrace is-finite)
  
  out)
(trace prove-tangent-vectors-3d-finite)

;; ---------------------------------------------------------------------------------------------------------------------
;; Prove Finiteness of the 3D Tangent Vectors (excluding the X-point) for a GK Geometry, using Automatic Differentiation
;; ---------------------------------------------------------------------------------------------------------------------
(define (prove-tangent-vectors-3d-finite-x-point geometry psi-coord
                                                 #:nx [nx 100]
                                                 #:x0 [x0 0.0]
                                                 #:x1 [x1 1.0]
                                                 #:ny [ny 100]
                                                 #:y0 [y0 0.0]
                                                 #:y1 [y1 1.0]
                                                 #:nz [nz 100]
                                                 #:z0 [z0 0.0]
                                                 #:z1 [z1 1.0])
  "Prove that the 3D tangent vectors remain finite everywhere (excluding the X-point) for the GK geometry specified by `geometry` using automatic differentiation.
  - `nx`: Number of cells in the x-direction.
  - `x0`, `x1`: Domain boundaries in the x-direction.
  - `ny`: Number of cells in the y-direction.
  - `y0`, `y1`: Domain boundaries in the y-direction.
  - `nz`: Number of cells in the z-direction.
  - `z0`, `z1`: Domain boundaries in the z-direction."

  (define exprs (hash-ref geometry 'exprs))
  (define coords (hash-ref geometry 'coords))
  (define func-exprs (hash-ref geometry 'func-exprs))

  (trace symbolic-diff)
  (trace symbolic-simp-rule)
  (trace symbolic-simp)
  (trace symbolic-tangents)
  (trace is-non-negative)
  (trace is-real)
  (trace is-non-zero)
  (trace is-finite)
  (trace is-finite-non-zero)

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
  
  (define tangent-vectors (symbolic-tangents exprs coords))
  (define tangent1-exprs (list-ref tangent-vectors 0))
  (define tangent2-exprs (list-ref tangent-vectors 1))
  (define tangent3-exprs (list-ref tangent-vectors 2))

  (define out (cond
                ;; Check whether the number of x-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nx 1) (>= x0 x1)) #f]

                ;; Check whether the number of y-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< ny 1) (>= y0 y1)) #f]

                ;; Check whether the number of z-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nz 1) (>= z0 z1)) #f]

                ;; Check whether the domain boundaries, any hence the coordinates, correspond to real numbers (otherwise, return false).
                [(or (not (is-real x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the domain boundaries, and hence the coordinates, are finite (otherwise, return false).
                [(or (not (is-finite x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the components of the first tangent vector e_1 are all finite, excluding the X-point (otherwise, return false).
                [(or (not (is-finite-non-zero (list-ref tangent1-exprs 0) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite-non-zero (list-ref tangent1-exprs 1) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite-non-zero (list-ref tangent1-exprs 2) (list psi-coord) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the second tangent vector e_2 are all finite, excluding the X-point (otherwise, return false).
                [(or (not (is-finite-non-zero (list-ref tangent2-exprs 0) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite-non-zero (list-ref tangent2-exprs 1) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite-non-zero (list-ref tangent2-exprs 2) (list psi-coord) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the third tangent vector e_3 are all finite, excluding the X-point (otherwise, return false).
                [(or (not (is-finite-non-zero (list-ref tangent3-exprs 0) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite-non-zero (list-ref tangent3-exprs 1) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-finite-non-zero (list-ref tangent3-exprs 2) (list psi-coord) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Otherwise, return true.
                [else #t]))

  (untrace symbolic-diff)
  (untrace symbolic-simp-rule)
  (untrace symbolic-simp)
  (untrace symbolic-tangents)
  (untrace is-non-negative)
  (untrace is-real)
  (untrace is-non-zero)
  (untrace is-finite)
  (untrace is-finite-non-zero)
  
  out)
(trace prove-tangent-vectors-3d-finite-x-point)

;; -------------------------------------------------------------------------------------------
;; Prove Realness of the 3D Tangent Vectors for a GK Geometry, using Automatic Differentiation
;; -------------------------------------------------------------------------------------------
(define (prove-tangent-vectors-3d-real geometry
                                       #:nx [nx 100]
                                       #:x0 [x0 0.0]
                                       #:x1 [x1 1.0]
                                       #:ny [ny 100]
                                       #:y0 [y0 0.0]
                                       #:y1 [y1 1.0]
                                       #:nz [nz 100]
                                       #:z0 [z0 0.0]
                                       #:z1 [z1 1.0])
  "Prove that the 3D tangent vectors remain real everywhere for the GK geometry specified by `geometry` using automatic differentiation.
  - `nx`: Number of cells in the x-direction.
  - `x0`, `x1`: Domain boundaries in the x-direction.
  - `ny`: Number of cells in the y-direction.
  - `y0`, `y1`: Domain boundaries in the y-direction.
  - `nz`: Number of cells in the z-direction.
  - `z0`, `z1`: Domain boundaries in the z-direction."

  (define exprs (hash-ref geometry 'exprs))
  (define coords (hash-ref geometry 'coords))
  (define func-exprs (hash-ref geometry 'func-exprs))

  (trace symbolic-diff)
  (trace symbolic-simp-rule)
  (trace symbolic-simp)
  (trace symbolic-tangents)
  (trace is-non-negative)
  (trace is-real)
  (trace is-non-zero)
  (trace is-finite)

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
  
  (define tangent-vectors (symbolic-tangents exprs coords))
  (define tangent1-exprs (list-ref tangent-vectors 0))
  (define tangent2-exprs (list-ref tangent-vectors 1))
  (define tangent3-exprs (list-ref tangent-vectors 2))

  (define out (cond
                ;; Check whether the number of x-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nx 1) (>= x0 x1)) #f]

                ;; Check whether the number of y-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< ny 1) (>= y0 y1)) #f]

                ;; Check whether the number of z-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nz 1) (>= z0 z1)) #f]

                ;; Check whether the domain boundaries, any hence the coordinates, correspond to real numbers (otherwise, return false).
                [(or (not (is-real x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the domain boundaries, and hence the coordinates, are finite (otherwise, return false).
                [(or (not (is-finite x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the components of the first tangent vector e_1 are all real (otherwise, return false).
                [(or (not (is-real (list-ref tangent1-exprs 0) func-exprs deriv-exprs-filtered coords))
                     (not (is-real (list-ref tangent1-exprs 1) func-exprs deriv-exprs-filtered coords))
                     (not (is-real (list-ref tangent1-exprs 2) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the second tangent vector e_2 are all real (otherwise, return false).
                [(or (not (is-real (list-ref tangent2-exprs 0) func-exprs deriv-exprs-filtered coords))
                     (not (is-real (list-ref tangent2-exprs 1) func-exprs deriv-exprs-filtered coords))
                     (not (is-real (list-ref tangent2-exprs 2) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the third tangent vector e_3 are all real (otherwise, return false).
                [(or (not (is-real (list-ref tangent3-exprs 0) func-exprs deriv-exprs-filtered coords))
                     (not (is-real (list-ref tangent3-exprs 1) func-exprs deriv-exprs-filtered coords))
                     (not (is-real (list-ref tangent3-exprs 2) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Otherwise, return true.
                [else #t]))

  (untrace symbolic-diff)
  (untrace symbolic-simp-rule)
  (untrace symbolic-simp)
  (untrace symbolic-tangents)
  (untrace is-non-negative)
  (untrace is-real)
  (untrace is-non-zero)
  (untrace is-finite)
  
  out)
(trace prove-tangent-vectors-3d-real)

;; -----------------------------------------------------------------------------------------------------------------------
;; Prove Realness of the 3D Tangent Vectors (from the X-point outwards) for a GK Geometry, using Automatic Differentiation
;; -----------------------------------------------------------------------------------------------------------------------
(define (prove-tangent-vectors-3d-real-x-point geometry psi-coord
                                               #:nx [nx 100]
                                               #:x0 [x0 0.0]
                                               #:x1 [x1 1.0]
                                               #:ny [ny 100]
                                               #:y0 [y0 0.0]
                                               #:y1 [y1 1.0]
                                               #:nz [nz 100]
                                               #:z0 [z0 0.0]
                                               #:z1 [z1 1.0])
  "Prove that the 3D tangent vectors remain real everywhere (from the X-point outwards) for the GK geometry specified by `geometry` using automatic differentiation.
  - `nx`: Number of cells in the x-direction.
  - `x0`, `x1`: Domain boundaries in the x-direction.
  - `ny`: Number of cells in the y-direction.
  - `y0`, `y1`: Domain boundaries in the y-direction.
  - `nz`: Number of cells in the z-direction.
  - `z0`, `z1`: Domain boundaries in the z-direction."

  (define exprs (hash-ref geometry 'exprs))
  (define coords (hash-ref geometry 'coords))
  (define func-exprs (hash-ref geometry 'func-exprs))

  (trace symbolic-diff)
  (trace symbolic-simp-rule)
  (trace symbolic-simp)
  (trace symbolic-tangents)
  (trace is-non-negative)
  (trace is-real-non-negative)
  (trace is-non-zero)
  (trace is-finite)

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
  
  (define tangent-vectors (symbolic-tangents exprs coords))
  (define tangent1-exprs (list-ref tangent-vectors 0))
  (define tangent2-exprs (list-ref tangent-vectors 1))
  (define tangent3-exprs (list-ref tangent-vectors 2))

  (define out (cond
                ;; Check whether the number of x-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nx 1) (>= x0 x1)) #f]

                ;; Check whether the number of y-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< ny 1) (>= y0 y1)) #f]

                ;; Check whether the number of z-direction cells is at least 1 and the right domain boundary is set to the right of the left boundary (otherwise, return false).
                [(or (< nz 1) (>= z0 z1)) #f]

                ;; Check whether the domain boundaries, any hence the coordinates, correspond to real numbers (otherwise, return false).
                [(or (not (is-real x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-real z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the domain boundaries, and hence the coordinates, are finite (otherwise, return false).
                [(or (not (is-finite x0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite x1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite y1 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z0 func-exprs deriv-exprs-filtered `()))
                     (not (is-finite z1 func-exprs deriv-exprs-filtered `()))) #f]

                ;; Check whether the components of the first tangent vector e_1 are all real, from the X-point outwards (otherwise, return false).
                [(or (not (is-real-non-negative (list-ref tangent1-exprs 0) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-real-non-negative (list-ref tangent1-exprs 1) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-real-non-negative (list-ref tangent1-exprs 2) (list psi-coord) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the second tangent vector e_2 are all real, from the X-point outwards (otherwise, return false).
                [(or (not (is-real-non-negative (list-ref tangent2-exprs 0) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-real-non-negative (list-ref tangent2-exprs 1) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-real-non-negative (list-ref tangent2-exprs 2) (list psi-coord) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Check whether the components of the third tangent vector e_3 are all real, from the X-point outwards (otherwise, return false).
                [(or (not (is-real-non-negative (list-ref tangent3-exprs 0) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-real-non-negative (list-ref tangent3-exprs 1) (list psi-coord) func-exprs deriv-exprs-filtered coords))
                     (not (is-real-non-negative (list-ref tangent3-exprs 2) (list psi-coord) func-exprs deriv-exprs-filtered coords))) #f]

                ;; Otherwise, return true.
                [else #t]))

  (untrace symbolic-diff)
  (untrace symbolic-simp-rule)
  (untrace symbolic-simp)
  (untrace symbolic-tangents)
  (untrace is-non-negative)
  (untrace is-real-non-negative)
  (untrace is-non-zero)
  (untrace is-finite)
  
  out)
(trace prove-tangent-vectors-3d-real-x-point)