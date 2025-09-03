#lang racket

(provide convert-expr
         remove-bracketed-expressions
         remove-bracketed-expressions-from-file
         flux-substitute
         train-lax-friedrichs-scalar-1d)

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

;; ---------------------------------------------------------------------
;; Train a Lax–Friedrichs (Finite-Difference) Solver for a 1D Scalar PDE
;; ---------------------------------------------------------------------
(define (train-lax-friedrichs-scalar-1d pde neural-net
                                        #:nx [nx 200]
                                        #:x0 [x0 0.0]
                                        #:x1 [x1 2.0]
                                        #:t-final [t-final 1.0]
                                        #:cfl [cfl 0.95]
                                        #:init-func [init-func `(cond
                                                                  [(< x 1.0) 1.0]
                                                                  [else 0.0])])
  "Generate C code that trains a solver for the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define cons-expr (hash-ref pde 'cons-expr))
  (define flux-expr (hash-ref pde 'flux-expr))
  (define max-speed-expr (hash-ref pde 'max-speed-expr))
  (define parameters (hash-ref pde 'parameters))

  (define max-trains (hash-ref neural-net 'max-trains))
  (define width (hash-ref neural-net 'width))
  (define depth (hash-ref neural-net 'depth))

  (define cons-code (convert-expr cons-expr))
  (define flux-code (convert-expr flux-expr))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))

  (define flux-um (flux-substitute flux-code cons-code "um"))
  (define flux-ui (flux-substitute flux-code cons-code "ui"))
  (define flux-up (flux-substitute flux-code cons-code "up"))

  (define max-speed-local (flux-substitute max-speed-code cons-code "u[i]"))

  (define parameter-code (cond
                           [(not (empty? parameters)) (string-join (map (lambda (parameter)
                                                                          (string-append "double " (convert-expr parameter) ";")) parameters) "\n")]
                           [else ""]))

  (define code
    (format "
// AUTO-GENERATED CODE FOR TRAINING ON SCALAR PDE: ~a
// Train a Lax–Friedrichs first-order finite-difference solver for a scalar PDE in 1D.

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

  // Neural network hyperparameters.
  const double num_trains = ~a;
  const int nn_width = ~a;
  const int nn_depth = ~a;

  // Arrays for storing solution.
  double *u = (double*) malloc((nx + 2) * sizeof(double));
  double *un = (double*) malloc((nx + 2) * sizeof(double));

  // Arrays for storing training data.
  float **input_data = (float**) malloc(nx * num_trains * sizeof(float*));
  float **output_data = (float**) malloc(nx * num_trains * sizeof(float*));

  // Initialize grid and set initial conditions.
  for (int i = 0; i <= nx + 1; i++) {
    double x = x0 + (i - 0.5) * dx;
    
    u[i] = ~a; // init-func in C.
  }

  // Initialize neural network architecture.
  kad_node_t *t_net;
  kann_t *ann;
  t_net = kann_layer_input(2);
  
  for (int i = 0; i < nn_depth; i++) {
    t_net = kann_layer_dense(t_net, nn_width);
    t_net = kad_tanh(t_net);
  }

  t_net = kann_layer_cost(t_net, 1, KANN_C_MSE);
  ann = kann_new(t_net, 0);

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

    // Compute fluxes with Lax-Friedrichs approximation and update the conserved variable.
    for (int i = 1; i <= nx; i++) {
      double um = u[i - 1];
      double ui = u[i];
      double up = u[i + 1];

      // Evaluate flux for each value of the conserved variable.
      double f_um = ~a; // f(u_{i - 1}).
      double f_ui = ~a; // f(u_i).
      double f_up = ~a; // f(u_{i + 1}).

      // Left interface flux: F_{i - 1/2} = 0.5 * (f(u_{i - 1}) + f(u_i)) - 0.5 * alpha * (u_i - u_{i - 1}).
      double fluxL = 0.5 * (f_um + f_ui) - 0.5 * alpha * (ui - um);

      // Right interface flux: F_{i + 1/2} = 0.5 * (f(u_{i + 1}) + f(u_i)) - 0.5 * alpha * (u_{i + 1} - u_i).
      double fluxR = 0.5 * (f_ui + f_up) - 0.5 * alpha * (up - ui);

      // Update the conserved variable.
      un[i] = ui - (dt / dx) * (fluxR - fluxL);
    }

    // Copy un -> u (updated conserved variables to new conserved variables).
    for (int i = 0; i <= nx + 1; i++) {
      u[i] = un[i];
    }

    // Apply simple boundary conditions (transmissive).
    u[0] = u[1];
    u[nx + 1] = u[nx];

    // Accumulate to training data.
    if (n < num_trains) {
      for (int i = 1; i <= nx; i++) {
        double x = x0 + (i - 0.5) * dx;

        input_data[(n * nx) + (i - 1)] = (float*) malloc(2 * sizeof(float));
        output_data[(n * nx) + (i - 1)] = (float*) malloc(sizeof(float));
      
        input_data[(n * nx) + (i - 1)][0] = t;
        input_data[(n * nx) + (i - 1)][1] = x;
        output_data[(n * nx) + (i - 1)][0] = u[i];
      }
    }

    // Output solution to disk.
    const char *fmt = \"output_%d.csv\";
    int sz = snprintf(0, 0, fmt, n);
    char file_nm[sz + 1];
    snprintf(file_nm, sizeof file_nm, fmt, n);
    
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

  // Train neural network.
  kann_train_fnn1(ann, 0.0001f, 64, 50, 10, 0.1f, n * nx, input_data, output_data);

  // Output neural network to disk.
  kann_save(\"neural_net.dat\", ann);

  free(u);
  free(un);

  kann_delete(ann);
  
  for (int i = 0; i < nx * num_trains; i++) {
    free(input_data[i]);
    free(output_data[i]);
  }

  free(input_data);
  free(output_data);
  
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
           ;; Maximum number of time-steps to train on.
           max-trains
           ;; Neural network width.
           width
           ;; Neural network depth.
           depth
           ;; Initial condition expression (e.g. (x < 1.0) ? 1.0 : 0.0)).
           init-func-code
           ;; Expression for local wave-speed estimate.
           max-speed-local
           ;; Left flux f(u_{i - 1}).
           flux-um
           ;; Middle flux f(u_i).
           flux-ui
           ;; Right flux f(u_{i + 1}).
           flux-up
           ))
  code)