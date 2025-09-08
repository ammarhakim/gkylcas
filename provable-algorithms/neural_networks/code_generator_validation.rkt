#lang racket

(provide validate-scalar-1d
         validate-scalar-1d-second-order
         validate-vector2-1d
         validate-vector2-1d-second-order
         validate-vector3-1d)

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

    ;; If expr is an absolute value of the form (abs expr1), then convert it to "fabs(expr1)" in C.
    [`(abs ,arg)
     (format "fabs(~a)" (convert-expr arg))]

    ;; If expr is a square root of the form (sqrt expr1), then convert it to "sqrt(expr1)" in C.
    [`(sqrt ,arg)
     (format "sqrt(~a)" (convert-expr arg))]

    ;; If expr is a maximum of the form (max expr1 expr2), then convert it to "fmax(expr1, expr2)" in C.
    [`(max ,arg1 ,arg2)
     (format "fmax(~a, ~a)" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a maximum of the form (max expr1 expr2 expr2), then convert it to "fmax(expr1, expr2, expr3)" in C.
    [`(max ,arg1 ,arg2 ,arg3)
     (format "fmax3(~a, ~a, ~a)" (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    ;; If expr is a minimum of the form (max expr1 expr2), then convert it to "fmin(expr1, expr2)" in C.
    [`(min ,arg1 ,arg2)
     (format "fmin(~a, ~a)" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a minimum of the form (max expr1 expr2 expr2), then convert it to "fmin(expr1, expr2, expr3)" in C.
    [`(min ,arg1 ,arg2 ,arg3)
     (format "fmin3(~a, ~a, ~a)" (convert-expr arg1) (convert-expr arg2) (convert-expr arg3))]

    ;; If expr is a variable assignment of the form (define expr1 expr2), then convert it to "expr1 = expr2" in C.
    [`(define ,arg1 ,arg2)
     (format "~a = ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a strict comparison of the form (< expr1 expr2), then convert it to "expr1 < expr2" in C.
    [`(< ,arg1 ,arg2)
     (format "~a < ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a comparison of the form (<= expr1 expr2), then convert it to "expr1 <= expr2" in C.
    [`(<= ,arg1 ,arg2)
     (format "~a <= ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a strict comparison of the form (> expr1 expr2), then convert it to "expr1 > expr2" in C.
    [`(> ,arg1 ,arg2)
     (format "~a > ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a comparison of the form (>= expr1 expr2), then convert it to "expr1 >= expr2" in C.
    [`(>= ,arg1 ,arg2)
     (format "~a >= ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is an equality comparison of the form (equal? expr1 expr2), then convert it to "expr1 == expr2" in C.
    [`(equal? ,arg1 ,arg2)
     (format "~a == ~a" (convert-expr arg1) (convert-expr arg2))]

    ;; If expr is a conditional of the form [(cond [cond1 expr1] [else expr2])], then convert it to the ternary operator "(cond1) ? expr1 : expr2" in C.
    [`(cond
        [,cond1 ,expr1]
        [else ,expr2])
     (format "(~a) ? ~a : ~a" (convert-expr cond1) (convert-expr expr1) (convert-expr expr2))]))

;; A simple boilerplate function for removing bracketed expressions from strings.
(define (remove-bracketed-expressions str)
  (regexp-replace* #rx"\\[.*?\\]" str ""))

;; A simple boilerplate function for removing bracketed expressions from files.
(define (remove-bracketed-expressions-from-file output-file)
  (define content
    (with-input-from-file output-file
      (lambda ()
        (port->string (current-input-port)))))
  (define cleaned
    (remove-bracketed-expressions content))
  (with-output-to-file output-file #:exists 'replace
    (lambda ()
      (display cleaned))))

(define (flux-substitute flux-expr cons-expr var-name)
  (string-replace flux-expr cons-expr var-name))

;; ------------------------------------------------------------------------
;; Validate an Arbitrary (First-Order) Surrogate Solver for a 1D Scalar PDE
;; ------------------------------------------------------------------------
(define (validate-scalar-1d pde neural-net
                            #:nx [nx 200]
                            #:x0 [x0 0.0]
                            #:x1 [x1 2.0]
                            #:t-final [t-final 1.0]
                            #:cfl [cfl 0.95]
                            #:init-func [init-func `(cond
                                                      [(< x 1.0) 1.0]
                                                      [else 0.0])])
  "Generate C code that validates a surrogate solver for the 1D scalar PDE specified by `pde` using any first-order method,
   with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define cons-expr (hash-ref pde 'cons-expr))
  (define max-speed-expr (hash-ref pde 'max-speed-expr))
  (define parameters (hash-ref pde 'parameters))

  (define cons-code (convert-expr cons-expr))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))
  
  (define max-speed-local (flux-substitute max-speed-code cons-code "u[i]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR VALIDATING ON SCALAR PDE: ~a
// Validate any first-order surrogate solver for a scalar PDE in 1D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include \"kann.h\"

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int nx = ~a;
  const double x0 = ~a;
  const double x1 = ~a;
  const double L = (x1 - x0);
  const double dx = L / nx;

  // Time-stepper setup.
  const double cfl = ~a;
  const double t_final = ~a;

  // Array for storing solution.
  double *u = (double*) malloc((nx + 2) * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  // Load neural network architecture.
  kann_t *ann;
  const char *fmt = \"%s_neural_net.dat\";
  int sz = snprintf(0, 0, fmt, \"~a\");
  char file_nm[sz + 1];
  snprintf(file_nm, sizeof file_nm, fmt, \"~a\");

  FILE *fptr;
  fptr = fopen(file_nm, \"r\");
  if (fptr != NULL) {
    ann = kann_load(file_nm);
    
    fclose(fptr);
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      double local_alpha = ~a; // max-speed-expr in C.
      
      if (local_alpha > alpha) {
        alpha = local_alpha;
      }
    }

    // Avoid division by zero.
    if (alpha < 1e-14) {
      alpha = 1e-14;
    }

    // Compute stable time step from alpha.
    double dt = cfl * dx / alpha;

    // If stepping beyond t_final, adjust dt accordingly.
    if (t + dt > t_final) {
      dt = t_final - t;
    }

    for (int i = 1; i <= nx; i++) {
      double x = x0 + (i - 0.5) * dx;
      
      float *input_data = (float*) malloc(2 * sizeof(float));
      const float *output_data;

      input_data[0] = t;
      input_data[1] = x;
      
      output_data = kann_apply1(ann, input_data);

      u[i] = output_data[0];

      free(input_data);
    }

    // Apply simple boundary conditions (transmissive).
    u[0] = u[1];
    u[nx + 1] = u[nx];

    // Output solution to disk.
    const char *fmt = \"%s_validation_%d.csv\";
    int sz = snprintf(0, 0, fmt, \"~a\", n);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", n);
    
    FILE *fptr;
    fptr = fopen(file_nm, \"w\");
    if (fptr != NULL) {
      for (int i = 1; i <= nx; i++) {
        double x = x0 + (i - 0.5) * dx;
        fprintf(fptr, \"%f, %f\\n\", x, u[i]);
      }
    }
    
    fclose(fptr);

    // Increment time.
    t += dt;
    n += 1;
  }

  free(u);
  kann_delete(ann);
  
  return 0;
}
"
           ;; PDE name for code comments.
           name
           ;; Additional PDE parameters (e.g. a = 1.0 for linear advection).
           parameter-code
           ;; Number of cells.
           nx
           ;; Left boundary.
           x0
           ;; Right boundary.
           x1
           ;; CFL coefficient.
           cfl
           ;; Final time.
           t-final
           ;; Initial condition expression (e.g. (x < 1.0) ? 1.0 : 0.0)).
           init-func-code
           ;; PDE name for neural network input.
           name
           name
           ;; Expression for local wave-speed estimate.
           max-speed-local
           ;; PDE name for file output.
           name
           name
           ))
  code)

;; -------------------------------------------------------------------------
;; Validate an Arbitrary (Second-Order) Surrogate Solver for a 1D Scalar PDE
;; -------------------------------------------------------------------------
(define (validate-scalar-1d-second-order pde limiter neural-net
                                         #:nx [nx 200]
                                         #:x0 [x0 0.0]
                                         #:x1 [x1 2.0]
                                         #:t-final [t-final 1.0]
                                         #:cfl [cfl 0.95]
                                         #:init-func [init-func `(cond
                                                                   [(< x 1.0) 1.0]
                                                                   [else 0.0])])
  "Generate C code that validates a surrogate solver for the 1D scalar PDE specified by `pde` using any first-order method with any second-order flux extrapolation
   using flux limiter `limiter`, with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define cons-expr (hash-ref pde 'cons-expr))
  (define max-speed-expr (hash-ref pde 'max-speed-expr))
  (define parameters (hash-ref pde 'parameters))

  (define limiter-name (hash-ref limiter 'name))

  (define cons-code (convert-expr cons-expr))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))
  
  (define max-speed-local (flux-substitute max-speed-code cons-code "u[i]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR VALIDATING ON SCALAR PDE: ~a
// FLUX LIMITER: ~a
// Validate any first-order surrogate solver for a scalar PDE in 1D, with any second-order flux extrapolation.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include \"kann.h\"

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int nx = ~a;
  const double x0 = ~a;
  const double x1 = ~a;
  const double L = (x1 - x0);
  const double dx = L / nx;

  // Time-stepper setup.
  const double cfl = ~a;
  const double t_final = ~a;

  // Array for storing solution.
  double *u = (double*) malloc((nx + 4) * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 3; i++) {
    double x = x0 + (i - 1.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  // Load neural network architecture.
  kann_t *ann;
  const char *fmt = \"%s_neural_net.dat\";
  int sz = snprintf(0, 0, fmt, \"~a\");
  char file_nm[sz + 1];
  snprintf(file_nm, sizeof file_nm, fmt, \"~a\");

  FILE *fptr;
  fptr = fopen(file_nm, \"r\");
  if (fptr != NULL) {
    ann = kann_load(file_nm);
    
    fclose(fptr);
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 2; i <= nx + 1; i++) {
      double local_alpha = ~a; // max-speed-expr in C.
      
      if (local_alpha > alpha) {
        alpha = local_alpha;
      }
    }

    // Avoid division by zero.
    if (alpha < 1e-14) {
      alpha = 1e-14;
    }

    // Compute stable time step from alpha.
    double dt = cfl * dx / alpha;

    // If stepping beyond t_final, adjust dt accordingly.
    if (t + dt > t_final) {
      dt = t_final - t;
    }

    for (int i = 2; i <= nx + 1; i++) {
      double x = x0 + (i - 1.5) * dx;
      
      float *input_data = (float*) malloc(2 * sizeof(float));
      const float *output_data;

      input_data[0] = t;
      input_data[1] = x;
      
      output_data = kann_apply1(ann, input_data);

      u[i] = output_data[0];

      free(input_data);
    }

    // Apply simple boundary conditions (transmissive).
    u[0] = u[2];
    u[1] = u[2];
    u[nx + 2] = u[nx + 1];
    u[nx + 3] = u[nx + 1];

    // Output solution to disk.
    const char *fmt = \"%s_validation_%d.csv\";
    int sz = snprintf(0, 0, fmt, \"~a\", n);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", n);
    
    FILE *fptr;
    fptr = fopen(file_nm, \"w\");
    if (fptr != NULL) {
      for (int i = 2; i <= nx + 1; i++) {
        double x = x0 + (i - 1.5) * dx;
        fprintf(fptr, \"%f, %f\\n\", x, u[i]);
      }
    }
    
    fclose(fptr);

    // Increment time.
    t += dt;
    n += 1;
  }

  free(u);
  kann_delete(ann);
  
  return 0;
}
"
           ;; PDE name for code comments.
           name
           ;; Flux limiter name for code comments.
           limiter-name
           ;; Additional PDE parameters (e.g. a = 1.0 for linear advection).
           parameter-code
           ;; Number of cells.
           nx
           ;; Left boundary.
           x0
           ;; Right boundary.
           x1
           ;; CFL coefficient.
           cfl
           ;; Final time.
           t-final
           ;; Initial condition expression (e.g. (x < 1.0) ? 1.0 : 0.0)).
           init-func-code
           ;; PDE name for neural network input.
           name
           name
           ;; Expression for local wave-speed estimate.
           max-speed-local
           ;; PDE name for file output.
           name
           name
           ))
  code)

;; ---------------------------------------------------------------------------------------------
;; Validate an Arbitrary (First-Order) Surrogate Solver for a 1D Coupled Vector System of 2 PDEs
;; ---------------------------------------------------------------------------------------------
(define (validate-vector2-1d pde-system neural-net
                             #:nx [nx 200]
                             #:x0 [x0 0.0]
                             #:x1 [x1 2.0]
                             #:t-final [t-final 1.0]
                             #:cfl [cfl 0.95]
                             #:init-funcs [init-funcs (list
                                                       `(cond
                                                          [(< x 0.5) 3.0]
                                                          [else 1.0])
                                                       `(cond
                                                          [(< x 0.5) 1.5]
                                                          [else 0.0]))])
  "Generate C code that validates a surrogate solver for the 1D coupled vector system of 2 PDEs specified by `pde` using any first-order method,
   with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-funcs`: Racket expressions for the initial conditions, e.g. piecewise constant."

  (define name (hash-ref pde-system 'name))
  (define cons-exprs (hash-ref pde-system 'cons-exprs))
  (define max-speed-exprs (hash-ref pde-system 'max-speed-exprs))
  (define parameters (hash-ref pde-system 'parameters))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define max-speed-codes (map (lambda (max-speed-expr)
                                 (convert-expr max-speed-expr)) max-speed-exprs))
  (define init-func-codes (map (lambda (init-func-expr)
                                 (convert-expr init-func-expr)) init-funcs))
  
  (define max-speed-locals (map (lambda (max-speed-code)
                                  (flux-substitute (flux-substitute max-speed-code (list-ref cons-codes 0) "u[(i * 2) + 0]")
                                                   (list-ref cons-codes 1) "u[(i * 2) + 1]")) max-speed-codes))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR VALIDATING ON COUPLED VECTOR PDE SYSTEM: ~a
// Validate any first-order surrogate solver for a coupled vector system of 2 PDEs in 1D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include \"kann.h\"

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int nx = ~a;
  const double x0 = ~a;
  const double x1 = ~a;
  const double L = (x1 - x0);
  const double dx = L / nx;

  // Time-stepper setup.
  const double cfl = ~a;
  const double t_final = ~a;

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 2) * 2 * sizeof(double));
  double *un = (double*) malloc((nx + 2) * 2 * sizeof(double));

  // Arrays for storing other intermediate values.
  double *local_alpha = (double*) malloc(2 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 2) + 1] = ~a; // init-funcs[1] in C.
  }

  // Load neural network architecture.
  kann_t **ann = (kann_t**) malloc(2 * sizeof(kann_t*));

  for (int i = 0; i < 2; i++) { 
    const char *fmt = \"%s_%d_neural_net.dat\";
    int sz = snprintf(0, 0, fmt, \"~a\", i);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", i);

    FILE *fptr;
    fptr = fopen(file_nm, \"r\");
    if (fptr != NULL) {
      ann[i] = kann_load(file_nm);
    
      fclose(fptr);
    }
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      local_alpha[0] = ~a; // max-speed-exprs[0] in C.
      local_alpha[1] = ~a; // max-speed-exprs[1] in C.
      
      for (int j = 0; j < 2; j++) {
        if (local_alpha[j] > alpha) {
          alpha = local_alpha[j];
        }
      }
    }

    // Avoid division by zero.
    if (alpha < 1e-14) {
      alpha = 1e-14;
    }

    // Compute stable time step from alpha.
    double dt = cfl * dx / alpha;

    // If stepping beyond t_final, adjust dt accordingly.
    if (t + dt > t_final) {
      dt = t_final - t;
    }

    for (int i = 1; i <= nx; i++) {
      for (int j = 0; j < 2; j++) {
        double x = x0 + (i - 0.5) * dx;
      
        float *input_data = (float*) malloc(2 * sizeof(float));
        const float *output_data;

        input_data[0] = t;
        input_data[1] = x;
      
        output_data = kann_apply1(ann[j], input_data);

        u[(i * 2) + j] = output_data[0];

        free(input_data);
      }
    }

    // Apply simple boundary conditions (transmissive).
    for (int j = 0; j < 2; j++) {
      u[(0 * 2) + j] = u[(1 * 2) + j];
      u[((nx + 1) * 2) + j] = u[(nx * 2) + j];
    }

    // Output solution to disk.
    for (int j = 0; j < 2; j++) {
      const char *fmt = \"%s_validation_%d_%d.csv\";
      int sz = snprintf(0, 0, fmt, \"~a\", j, n);
      char file_nm[sz + 1];
      snprintf(file_nm, sizeof file_nm, fmt, \"~a\", j, n);
    
      FILE *fptr;
      fptr = fopen(file_nm, \"w\");
      if (fptr != NULL) {
        for (int i = 1; i <= nx; i++) {
          double x = x0 + (i - 0.5) * dx;
          fprintf(fptr, \"%f, %f\\n\", x, u[(i * 2) + j]);
        }
      }
      
      fclose(fptr);
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  free(u);
  free(local_alpha);
  
  for (int i = 0; i < 2; i++) {
    kann_delete(ann[i]);
  }
  free(ann);
  
  return 0;
}
"
           ;; PDE name for code comments.
           name
           ;; Additional PDE parameters (e.g. a = 1.0 for linear advection).
           parameter-code
           ;; Number of cells.
           nx
           ;; Left boundary.
           x0
           ;; Right boundary.
           x1
           ;; CFL coefficient.
           cfl
           ;; Final time.
           t-final
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           ;; PDE name for neural network input.
           name
           name
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           ;; PDE name for file output.
           name
           name
           ))
  code)

;; ----------------------------------------------------------------------------------------------
;; Validate an Arbitrary (Second-Order) Surrogate Solver for a 1D Coupled Vector System of 2 PDEs
;; ----------------------------------------------------------------------------------------------
(define (validate-vector2-1d-second-order pde-system limiter neural-net
                                          #:nx [nx 200]
                                          #:x0 [x0 0.0]
                                          #:x1 [x1 2.0]
                                          #:t-final [t-final 1.0]
                                          #:cfl [cfl 0.95]
                                          #:init-funcs [init-funcs (list
                                                                    `(cond
                                                                       [(< x 0.5) 3.0]
                                                                       [else 1.0])
                                                                    `(cond
                                                                       [(< x 0.5) 1.5]
                                                                       [else 0.0]))])
  "Generate C code that validates a surrogate solver for the 1D coupled vector system of 2 PDEs specified by `pde` using any first-order method
   with any second-order flux extrapolation using flux limiter `limiter`, with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-funcs`: Racket expressions for the initial conditions, e.g. piecewise constant."

  (define name (hash-ref pde-system 'name))
  (define cons-exprs (hash-ref pde-system 'cons-exprs))
  (define max-speed-exprs (hash-ref pde-system 'max-speed-exprs))
  (define parameters (hash-ref pde-system 'parameters))

  (define limiter-name (hash-ref limiter 'name))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define max-speed-codes (map (lambda (max-speed-expr)
                                 (convert-expr max-speed-expr)) max-speed-exprs))
  (define init-func-codes (map (lambda (init-func-expr)
                                 (convert-expr init-func-expr)) init-funcs))
  
  (define max-speed-locals (map (lambda (max-speed-code)
                                  (flux-substitute (flux-substitute max-speed-code (list-ref cons-codes 0) "u[(i * 2) + 0]")
                                                   (list-ref cons-codes 1) "u[(i * 2) + 1]")) max-speed-codes))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR VALIDATING ON COUPLED VECTOR PDE SYSTEM: ~a
// FLUX LIMITER: ~a
// Validate any first-order surrogate solver for a coupled vector system of 2 PDEs in 1D, with a second-order flux extrapolation.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include \"kann.h\"

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int nx = ~a;
  const double x0 = ~a;
  const double x1 = ~a;
  const double L = (x1 - x0);
  const double dx = L / nx;

  // Time-stepper setup.
  const double cfl = ~a;
  const double t_final = ~a;

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 4) * 2 * sizeof(double));
  double *un = (double*) malloc((nx + 4) * 2 * sizeof(double));

  // Arrays for storing other intermediate values.
  double *local_alpha = (double*) malloc(2 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 3; i++) {
    double x = x0 + (i - 1.5) * dx;
    
    u[(i * 2) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 2) + 1] = ~a; // init-funcs[1] in C.
  }

  // Load neural network architecture.
  kann_t **ann = (kann_t**) malloc(2 * sizeof(kann_t*));

  for (int i = 0; i < 2; i++) { 
    const char *fmt = \"%s_%d_neural_net.dat\";
    int sz = snprintf(0, 0, fmt, \"~a\", i);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", i);

    FILE *fptr;
    fptr = fopen(file_nm, \"r\");
    if (fptr != NULL) {
      ann[i] = kann_load(file_nm);
    
      fclose(fptr);
    }
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 1; i <= nx + 2; i++) {
      local_alpha[0] = ~a; // max-speed-exprs[0] in C.
      local_alpha[1] = ~a; // max-speed-exprs[1] in C.
      
      for (int j = 0; j < 2; j++) {
        if (local_alpha[j] > alpha) {
          alpha = local_alpha[j];
        }
      }
    }

    // Avoid division by zero.
    if (alpha < 1e-14) {
      alpha = 1e-14;
    }

    // Compute stable time step from alpha.
    double dt = cfl * dx / alpha;

    // If stepping beyond t_final, adjust dt accordingly.
    if (t + dt > t_final) {
      dt = t_final - t;
    }

    for (int i = 2; i <= nx + 1; i++) {
      for (int j = 0; j < 2; j++) {
        double x = x0 + (i - 1.5) * dx;
      
        float *input_data = (float*) malloc(2 * sizeof(float));
        const float *output_data;

        input_data[0] = t;
        input_data[1] = x;
      
        output_data = kann_apply1(ann[j], input_data);

        u[(i * 2) + j] = output_data[0];

        free(input_data);
      }
    }

    // Apply simple boundary conditions (transmissive).
    for (int j = 0; j < 2; j++) {
      u[(0 * 2) + j] = u[(2 * 2) + j];
      u[(1 * 2) + j] = u[(2 * 2) + j];
      u[((nx + 2) * 2) + j] = u[((nx + 1) * 2) + j];
      u[((nx + 3) * 2) + j] = u[((nx + 1) * 2) + j];
    }

    // Output solution to disk.
    for (int j = 0; j < 2; j++) {
      const char *fmt = \"%s_validation_%d_%d.csv\";
      int sz = snprintf(0, 0, fmt, \"~a\", j, n);
      char file_nm[sz + 1];
      snprintf(file_nm, sizeof file_nm, fmt, \"~a\", j, n);
    
      FILE *fptr;
      fptr = fopen(file_nm, \"w\");
      if (fptr != NULL) {
        for (int i = 2; i <= nx + 1; i++) {
          double x = x0 + (i - 1.5) * dx;
          fprintf(fptr, \"%f, %f\\n\", x, u[(i * 2) + j]);
        }
      }
      
      fclose(fptr);
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  free(u);
  free(local_alpha);
  
  for (int i = 0; i < 2; i++) {
    kann_delete(ann[i]);
  }
  free(ann);
  
  return 0;
}
"
           ;; PDE name for code comments.
           name
           ;; Flux limiter name for code comments.
           limiter-name
           ;; Additional PDE parameters (e.g. a = 1.0 for linear advection).
           parameter-code
           ;; Number of cells.
           nx
           ;; Left boundary.
           x0
           ;; Right boundary.
           x1
           ;; CFL coefficient.
           cfl
           ;; Final time.
           t-final
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           ;; PDE name for neural network input.
           name
           name
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           ;; PDE name for file output.
           name
           name
           ))
  code)

;; ---------------------------------------------------------------------------------------------
;; Validate an Arbitrary (First-Order) Surrogate Solver for a 1D Coupled Vector System of 3 PDEs
;; ---------------------------------------------------------------------------------------------
(define (validate-vector3-1d pde-system neural-net
                             #:nx [nx 200]
                             #:x0 [x0 0.0]
                             #:x1 [x1 2.0]
                             #:t-final [t-final 1.0]
                             #:cfl [cfl 0.95]
                             #:init-funcs [init-funcs (list
                                                       `(cond
                                                          [(< x 0.5) 3.0]
                                                          [else 1.0])
                                                       `(cond
                                                          [(< x 0.5) 0.0]
                                                          [else 0.0])
                                                       `(cond
                                                          [(< x 0.5) 7.5]
                                                          [else 2.5]))])
  "Generate C code that validates a surrogate solver for the 1D coupled vector system of 3 PDEs specified by `pde` using any first-order method,
   with neural network architecture `neural-net`.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-funcs`: Racket expressions for the initial conditions, e.g. piecewise constant."

  (define name (hash-ref pde-system 'name))
  (define cons-exprs (hash-ref pde-system 'cons-exprs))
  (define max-speed-exprs (hash-ref pde-system 'max-speed-exprs))
  (define parameters (hash-ref pde-system 'parameters))

  (define cons-codes (map (lambda (cons-expr)
                            (convert-expr cons-expr)) cons-exprs))
  (define max-speed-codes (map (lambda (max-speed-expr)
                                 (convert-expr max-speed-expr)) max-speed-exprs))
  (define init-func-codes (map (lambda (init-func-expr)
                                 (convert-expr init-func-expr)) init-funcs))
  
  (define max-speed-locals (map (lambda (max-speed-code)
                                  (flux-substitute (flux-substitute (flux-substitute max-speed-code (list-ref cons-codes 0) "u[(i * 3) + 0]")
                                                                    (list-ref cons-codes 1) "u[(i * 3) + 1]") (list-ref cons-codes 2) "u[(i * 3) + 2]")) max-speed-codes))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR VALIDATING ON COUPLED VECTOR PDE SYSTEM: ~a
// Validate any first-order surrogate solver for a coupled vector system of 3 PDEs in 1D.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include \"kann.h\"

// Additional PDE parameters (if any).
~a

int main() {
  // Spatial domain setup.
  const int nx = ~a;
  const double x0 = ~a;
  const double x1 = ~a;
  const double L = (x1 - x0);
  const double dx = L / nx;

  // Time-stepper setup.
  const double cfl = ~a;
  const double t_final = ~a;

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 2) * 3 * sizeof(double));
  double *un = (double*) malloc((nx + 2) * 3 * sizeof(double));

  // Arrays for storing other intermediate values.
  double *local_alpha = (double*) malloc(3 * sizeof(double));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[(i * 3) + 0] = ~a; // init-funcs[0] in C.
    u[(i * 3) + 1] = ~a; // init-funcs[1] in C.
    u[(i * 3) + 2] = ~a; // init-funcs[2] in C.
  }

  // Load neural network architecture.
  kann_t **ann = (kann_t**) malloc(3 * sizeof(kann_t*));

  for (int i = 0; i < 3; i++) { 
    const char *fmt = \"%s_%d_neural_net.dat\";
    int sz = snprintf(0, 0, fmt, \"~a\", i);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, \"~a\", i);

    FILE *fptr;
    fptr = fopen(file_nm, \"r\");
    if (fptr != NULL) {
      ann[i] = kann_load(file_nm);
    
      fclose(fptr);
    }
  }

  double t = 0.0;
  int n = 0;
  while (t < t_final) {
    // Determine global maximum wave-speed alpha (for stable dt).
    // Simplistic approach: we compute the local alpha for each cell and take the maximum over the entire domain.
    double alpha = 0.0;
    
    for (int i = 1; i <= nx; i++) {
      local_alpha[0] = ~a; // max-speed-exprs[0] in C.
      local_alpha[1] = ~a; // max-speed-exprs[1] in C.
      local_alpha[2] = ~a; // max-speed-exprs[2] in C.
      
      for (int j = 0; j < 3; j++) {
        if (local_alpha[j] > alpha) {
          alpha = local_alpha[j];
        }
      }
    }

    // Avoid division by zero.
    if (alpha < 1e-14) {
      alpha = 1e-14;
    }

    // Compute stable time step from alpha.
    double dt = cfl * dx / alpha;

    // If stepping beyond t_final, adjust dt accordingly.
    if (t + dt > t_final) {
      dt = t_final - t;
    }

    for (int i = 1; i <= nx; i++) {
      for (int j = 0; j < 3; j++) {
        double x = x0 + (i - 0.5) * dx;
      
        float *input_data = (float*) malloc(2 * sizeof(float));
        const float *output_data;

        input_data[0] = t;
        input_data[1] = x;
      
        output_data = kann_apply1(ann[j], input_data);

        u[(i * 3) + j] = output_data[0];

        free(input_data);
      }
    }

    // Apply simple boundary conditions (transmissive).
    for (int j = 0; j < 3; j++) {
      u[(0 * 3) + j] = u[(1 * 3) + j];
      u[((nx + 1) * 3) + j] = u[(nx * 3) + j];
    }

    // Output solution to disk.
    for (int j = 0; j < 3; j++) {
      const char *fmt = \"%s_validation_%d_%d.csv\";
      int sz = snprintf(0, 0, fmt, \"~a\", j, n);
      char file_nm[sz + 1];
      snprintf(file_nm, sizeof file_nm, fmt, \"~a\", j, n);
    
      FILE *fptr;
      fptr = fopen(file_nm, \"w\");
      if (fptr != NULL) {
        for (int i = 1; i <= nx; i++) {
          double x = x0 + (i - 0.5) * dx;
          fprintf(fptr, \"%f, %f\\n\", x, u[(i * 3) + j]);
        }
      }
      
      fclose(fptr);
    }

    // Increment time.
    t += dt;
    n += 1;
  }

  free(u);
  free(local_alpha);
  
  for (int i = 0; i < 3; i++) {
    kann_delete(ann[i]);
  }
  free(ann);
  
  return 0;
}
"
           ;; PDE name for code comments.
           name
           ;; Additional PDE parameters (e.g. a = 1.0 for linear advection).
           parameter-code
           ;; Number of cells.
           nx
           ;; Left boundary.
           x0
           ;; Right boundary.
           x1
           ;; CFL coefficient.
           cfl
           ;; Final time.
           t-final
           ;; Initial condition expressions (e.g. (x < 1.0) ? 1.0 : 0.0)).
           (list-ref init-func-codes 0)
           (list-ref init-func-codes 1)
           (list-ref init-func-codes 2)
           ;; PDE name for neural network input.
           name
           name
           ;; Expressions for local wave-speed estimates.
           (list-ref max-speed-locals 0)
           (list-ref max-speed-locals 1)
           (list-ref max-speed-locals 2)
           ;; PDE name for file output.
           name
           name
           ))
  code)