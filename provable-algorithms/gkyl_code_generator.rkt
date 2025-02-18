#lang racket

(require "code_generator.rkt")
(require "prover.rkt")
(provide remove-bracketed-expressions
         remove-bracketed-expressions-from-file
         gkyl-generate-lax-friedrichs-scalar-1d-header
         gkyl-generate-lax-friedrichs-scalar-1d-priv-header
         gkyl-generate-lax-friedrichs-scalar-1d-source
         gkyl-generate-roe-scalar-1d-header
         gkyl-generate-roe-scalar-1d-priv-header
         gkyl-generate-roe-scalar-1d-source)

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

;; -------------------------------------------------------------------------------
;; Header for Gkeyll Lax–Friedrichs (Finite-Difference) Solver for a 1D Scalar PDE
;; -------------------------------------------------------------------------------
(define (gkyl-generate-lax-friedrichs-scalar-1d-header pde
                                                       #:nx [nx 200]
                                                       #:x0 [x0 0.0]
                                                       #:x1 [x1 2.0]
                                                       #:t-final [t-final 1.0]
                                                       #:cfl [cfl 0.95]
                                                       #:init-func [init-func `(cond
                                                                                 [(< x 1.0) 1.0]
                                                                                 [else 0.0])])
 "Generate Gkeyll C header code that solves the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define parameters (hash-ref pde 'parameters))

  (define parameter-def (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) "; // Additional simulation parameter.")]
                          [else ""]))
  (define parameter-sig (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) ",")]
                          [else ""]))
  (define parameter-comment (cond
                              [(not (empty? parameters)) (string-append "* @param " (convert-expr (list-ref parameters 1)) " Additional simulation parameter.")]
                              [else "*"]))

  (define code
    (format "
#pragma once

#include <gkyl_wv_eqn.h>

// Type of Riemann-solver to use:
enum gkyl_wv_~a_rp {
  WV_~a_RP_LAX = 0, // Default (Lax fluxes).
};

// Input context, packaged as a struct.
struct gkyl_wv_~a_inp {
  ~a

  enum gkyl_wv_~a_rp rp_type; // Type of Riemann-solver to use.
  bool use_gpu; // Whether the wave equation object is on the host (false) or the device (true).
};

/**
* Create a new ~a equations object.
*
~a
* @param use_gpu Whether the wave equation object is on the host (false) or the device (true).
* @return Pointer to the ~a equations object.
*/
struct gkyl_wv_eqn*
gkyl_wv_~a_new(~a bool use_gpu);

/**
* Create a new ~a equations object, from an input context struct.
*
* @param inp Input context struct.
* @return Pointer to the ~a equations object.
*/
struct gkyl_wv_eqn*
gkyl_wv_~a_inew(const struct gkyl_wv_~a_inp* inp);
"
           name
           (string-upcase name)
           name
           parameter-def
           name
           name
           parameter-comment
           name
           name
           parameter-sig
           name
           name
           name
           name
           ))
  code)

;; ---------------------------------------------------------------------------------------
;; Private Header for Gkeyll Lax–Friedrichs (Finite-Difference) Solver for a 1D Scalar PDE
;; ---------------------------------------------------------------------------------------
(define (gkyl-generate-lax-friedrichs-scalar-1d-priv-header pde
                                                            #:nx [nx 200]
                                                            #:x0 [x0 0.0]
                                                            #:x1 [x1 2.0]
                                                            #:t-final [t-final 1.0]
                                                            #:cfl [cfl 0.95]
                                                            #:init-func [init-func `(cond
                                                                                      [(< x 1.0) 1.0]
                                                                                      [else 0.0])])
 "Generate Gkeyll C private header code that solves the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define parameters (hash-ref pde 'parameters))

  (define parameter-def (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) "; // Additional simulation parameter.")]
                          [else ""]))
  (define parameter-sig (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) ",")]
                          [else ""]))
  (define parameter-comment (cond
                              [(not (empty? parameters)) (string-append "* @param " (convert-expr (list-ref parameters 1)) " Additional simulation parameter.")]
                              [else "*"]))

  (define code
    (format "
#pragma once

// Private header, not for direct use in user-facing code.

#include <math.h>
#include <gkyl_array.h>
#include <gkyl_wv_eqn.h>
#include <gkyl_eqn_type.h>
#include <gkyl_range.h>
#include <gkyl_util.h>

struct wv_~a {
  struct gkyl_wv_eqn eqn; // Base equation object.
  ~a
};

/**
* Compute maximum absolute wave speed.
*
~a
* @param q Conserved variable vector.
* @return Maximum absolute wave speed for a given q.
*/
GKYL_CU_D
static inline double
gkyl_~a_max_abs_speed(~a const double* q);

/**
* Compute flux vector. Assumes rotation to local coordinate system.
*
~a
* @param q Conserved variable vector.
* @param flux Flux vector in direction 'dir' (output).
*/
GKYL_CU_D
void
gkyl_~a_flux(~a const double* q, double* flux);

/**
* Compute Riemann variables given the conserved variables.
*
* @param eqn Base equation object.
* @param qstate Current state vector.
* @param qin Conserved variable vector (input).
* @param wout Riemann variable vector (output).
*/
GKYL_CU_D
static inline void
cons_to_riem(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* qin, double* wout);

/**
* Compute conserved variables given the Riemann variables.
*
* @param eqn Base equation object.
* @param qstate Current state vector.
* @param win Riemann variable vector (input).
* @param qout Conserved variable vector (output).
*/
GKYL_CU_D
static inline void
riem_to_cons(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* win, double *qout);

/**
* Boundary condition function for applying wall boundary conditions for the ~a equations.
*
* @param eqn Base equation object.
* @param t Current simulation time.
* @param nc Number of boundary cells to which to apply wall boundary conditions.
* @param skin Skin cells in boundary region (from which values are copied).
* @param ghost Ghost cells in boundary region (to which values are copied).
* @param ctx Context to pass to the function.
*/
GKYL_CU_D
static void
~a_wall(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx);

/**
* Boundary condition function for applying no-slip boundary conditions for the ~a equations.
*
* @param eqn Base equation object.
* @param t Current simulation time.
* @param nc Number of boundary cells to which to apply no-slip boundary conditions.
* @param skin Skin cells in boundary region (from which values are copied).
* @param ghost Ghost cells in boundary region (to which values are copied).
* @param ctx Context to pass to the function.
*/
GKYL_CU_D
static void
~a_no_slip(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx);

/**
* Rotate state vector from global to local coordinate frame.
*
* @param eqn Base equation object.
* @param tau1 First tangent vector of the coordinate frame.
* @param tau2 Second tangent vector of the coordinate frame.
* @param norm Normal vector of the coordinate frame.
* @param qglobal State vector in global coordinate frame (input).
* @param qlocal State vector in local coordinate frame (output).
*/
GKYL_CU_D
static inline void
rot_to_local(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qglobal,
  double* GKYL_RESTRICT qlocal);

/**
* Rotate state vector from local to global coordinate frame.
*
* @param eqn Base equation object.
* @param tau1 First tangent vector of the coordinate frame.
* @param tau2 Second tangent vector of the coordinate frame.
* @param norm Normal vector of the coordinate frame.
* @param qlocal State vector in local coordinate frame (input).
* @param qglobal State vector in global coordinate frame (output).
*/
GKYL_CU_D
static inline void
rot_to_global(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qlocal,
  double* GKYL_RESTRICT qglobal);

/**
* Compute waves and speeds using Lax fluxes.
*
* @param eqn Base equation object.
* @param delta Jump across interface to split.
* @param ql Conserved variables on the left of the interface.
* @param qr Conserved variables on the right of the interface.
* @param waves Waves (output).
* @param s Wave speeds (output).
* @return Maximum wave speed.
*/
GKYL_CU_D
static double
wave_lax(const struct gkyl_wv_eqn* eqn, const double* delta, const double* ql, const double* qr, double* waves, double* s);

/**
* Compute fluctuations using Lax fluxes.
*
* @param eqn Base equation object.
* @param ql Conserved variable vector on the left of the interface.
* @param qr Conserved variable vector on the right of the interface.
* @param waves Waves (input).
* @param s Wave speeds (input).
* @param amdq Left-moving fluctuations (output).
* @param apdq Right-moving fluctuations (output).
*/
GKYL_CU_D
static void
qfluct_lax(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, const double* waves, const double* s, double* amdq, double* apdq);

/**
* Compute waves and speeds using Lax fluxes (with potential fallback).
*
* @param eqn Base equation object.
* @param type Type of Riemann-solver flux to use.
* @param delta Jump across interface to split.
* @param ql Conserved variables on the left of the interface.
* @param qr Conserved variables on the right of the interface.
* @param waves Waves (output).
* @param s Wave speeds (output).
* @return Maximum wave speed.
*/
GKYL_CU_D
static double
wave_lax_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* delta, const double* ql, const double* qr, double* waves, double* s);

/**
* Compute fluctuations using Lax fluxes (with potential fallback),
*
* @param eqn Base equation object.
* @param type Type of Riemann-solver flux to use.
* @param ql Conserved variable vector on the left of the interface.
* @param qr Conserved variable vector on the right of the interface.
* @param waves Waves (input).
* @param s Wave speeds (input).
* @param amdq Left-moving fluctuations (output).
* @param apdq Right-moving fluctuations (output).
*/
GKYL_CU_D
static void
qfluct_lax_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* ql, const double* qr, const double* waves, const double* s,
  double* amdq, double* apdq);

/**
* Compute jump in flux given two conserved variable states.
*
* @param eqn Base equation object.
* @param ql Conserved variable vector on the left of the interface (input).
* @param qr Conserved variable vector on the right of the interface (input).
* @param flux_jump Jump in flux vector (output).
* @return Maximum wave speeds for states ql and qr.
*/
GKYL_CU_D
static double
flux_jump(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, double* flux_jump);

/**
* Determine whether invariant domain of the ~a equations is satisfied.
*
* @param eqn Base equation object.
* @param q Conserved variable vector.
* @return Whether the invariant domain is satisfied.
*/
GKYL_CU_D
static bool
check_inv(const struct gkyl_wv_eqn* eqn, const double* q);

/**
* Compute maximum wave speed from a conserved variable vector.
*
* @param eqn Base equation object.
* @param q Conserved variable vector.
* @return Maximum absolute wave speed.
*/
GKYL_CU_D
static double
max_speed(const struct gkyl_wv_eqn* eqn, const double* q);

/**
* Convert conserved variables to diagnostic variables.
*
* @param eqn Base equation object.
* @param qin Conserved variable vector (input).
* @param diag Diagnostic variable vector (output).
*/
GKYL_CU_D
static inline void
~a_cons_to_diag(const struct gkyl_wv_eqn* eqn, const double* qin, double* diag);

/**
* Compute forcing/source term vector from conserved variable vector.
*
* @param eqn Base equation object.
* @param qin Conserved variable vector (input).
* @param sout Forcing/source term vector (output).
*/
GKYL_CU_DH
static inline void
~a_source(const struct gkyl_wv_eqn* eqn, const double* qin, double* sout);

/**
* Free ~a equations object.
*
* @param ref Reference counter for ~a equations.
*/
void gkyl_~a_free(const struct gkyl_ref_count* ref);
"
           name
           parameter-def
           parameter-comment
           name
           parameter-sig
           parameter-comment
           name
           parameter-sig
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           ))
  code)

;; -------------------------------------------------------------------------------
;; Source for Gkeyll Lax–Friedrichs (Finite-Difference) Solver for a 1D Scalar PDE
;; -------------------------------------------------------------------------------
(define (gkyl-generate-lax-friedrichs-scalar-1d-source pde
                                                       #:nx [nx 200]
                                                       #:x0 [x0 0.0]
                                                       #:x1 [x1 2.0]
                                                       #:t-final [t-final 1.0]
                                                       #:cfl [cfl 0.95]
                                                       #:init-func [init-func `(cond
                                                                                 [(< x 1.0) 1.0]
                                                                                 [else 0.0])])
 "Generate Gkeyll C source code that solves the 1D scalar PDE specified by `pde` using the Lax-Friedrichs finite-difference method.
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

  (define cons-code (convert-expr cons-expr))
  (define flux-code (convert-expr flux-expr))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))

  (define max-speed-local (flux-substitute max-speed-code cons-code "q[0]"))
  (define flux-ui (flux-substitute flux-code cons-code "q[0]"))
  
  (define parameter-def (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) " = "
                                                                    name "->" (convert-expr (list-ref parameters 1)) "; // Additional simulation parameter.")]
                          [else ""]))
  (define parameter-sig (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) ",")]
                          [else ""]))
  (define parameter-name (cond
                           [(not (empty? parameters)) (string-append (convert-expr (list-ref parameters 1)) ",")]
                           [else ""]))
  (define parameter-field (cond
                            [(not (empty? parameters)) (string-append "." (convert-expr (list-ref parameters 1)) " = "
                                                                      (convert-expr (list-ref parameters 1)) ",")]
                            [else ""]))
  (define parameter-field-set (cond
                                [(not (empty? parameters)) (string-append name "->" (convert-expr (list-ref parameters 1))
                                                                          " = inp->" (convert-expr (list-ref parameters 1)) ";")]
                                [else ""]))

  (define code
    (format "
#include <assert.h>
#include <math.h>

#include <gkyl_alloc.h>
#include <gkyl_alloc_flags_priv.h>
#include <gkyl_wv_~a.h>
#include <gkyl_wv_~a_priv.h>

static inline double
gkyl_~a_max_abs_speed(~a const double* q)
{
  return ~a;
}

void
gkyl_~a_flux(~a const double* q, double* flux)
{
  flux[0] = ~a;
}

static inline void
cons_to_riem(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* qin, double* wout)
{
  // TODO: This should use a proper L matrix.
  wout[0] = qin[0];
}

static inline void
riem_to_cons(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* win, double* qout)
{
  // TODO: This should use a proper L matrix.
  qout[0] = win[0];
}

static void
~a_wall(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx)
{
  ghost[0] = skin[0];
}

static void
~a_no_slip(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx)
{
  ghost[0] = skin[0];
}

static inline void
rot_to_local(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qglobal,
  double* GKYL_RESTRICT qlocal)
{
  qlocal[0] = qglobal[0];
}

static inline void
rot_to_global(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qlocal,
  double* GKYL_RESTRICT qglobal)
{
  qglobal[0] = qlocal[0];
}

static double
wave_lax(const struct gkyl_wv_eqn* eqn, const double* delta, const double* ql, const double* qr, double* waves, double* s)
{
  const struct wv_~a *~a = container_of(eqn, struct wv_~a, eqn);
  ~a

  double sl = gkyl_~a_max_abs_speed(~a ql);
  double sr = gkyl_~a_max_abs_speed(~a qr);
  double amax = fmax(sl, sr);

  double *fl = gkyl_malloc(sizeof(double));
  double *fr = gkyl_malloc(sizeof(double));
  gkyl_~a_flux(~a ql, fl);
  gkyl_~a_flux(~a qr, fr);

  double *w0 = &waves[0], *w1 = &waves[1];
  w0[0] = 0.5 * ((qr[0] - ql[0]) - (fr[0] - fl[0]) / amax);
  w1[0] = 0.5 * ((qr[0] - ql[0]) + (fr[0] - fl[0]) / amax);

  s[0] = -amax;
  s[1] = amax;

  gkyl_free(fl);
  gkyl_free(fr);

  return s[1];
}

static void
qfluct_lax(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, const double* waves, const double* s, double* amdq, double* apdq)
{
  const double *w0 = &waves[0], *w1 = &waves[1];
  double s0m = fmin(0.0, s[0]), s1m = fmin(0.0, s[1]);
  double s0p = fmax(0.0, s[0]), s1p = fmax(0.0, s[1]);

  amdq[0] = (s0m * w0[0]) + (s1m * w1[0]);
  apdq[0] = (s0p * w0[0]) + (s1p * w1[0]);
}

static double
wave_lax_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* delta, const double* ql, const double* qr, double* waves, double* s)
{
  return wave_lax(eqn, delta, ql, qr, waves, s);
}

static void
qfluct_lax_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* ql, const double* qr, const double* waves, const double* s,
  double* amdq, double* apdq)
{
  return qfluct_lax(eqn, ql, qr, waves, s, amdq, apdq);
}

static double
flux_jump(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, double* flux_jump)
{
  const struct wv_~a *~a = container_of(eqn, struct wv_~a, eqn);
  ~a

  double *fr = gkyl_malloc(sizeof(double));
  double *fl = gkyl_malloc(sizeof(double));
  gkyl_~a_flux(~a ql, fl);
  gkyl_~a_flux(~a qr, fr);

  flux_jump[0] = fr[0] - fl[0];

  double amaxl = gkyl_~a_max_abs_speed(~a ql);
  double amaxr = gkyl_~a_max_abs_speed(~a qr);
  
  gkyl_free(fr);
  gkyl_free(fl);

  return fmax(amaxl, amaxr);
}

static bool
check_inv(const struct gkyl_wv_eqn* eqn, const double* q)
{
  return true; // All states are assumed to be valid.
}

static double
max_speed(const struct gkyl_wv_eqn* eqn, const double* q)
{
  const struct wv_~a *~a = container_of(eqn, struct wv_~a, eqn);
  ~a

  return gkyl_~a_max_abs_speed(~a q);
}

static inline void
~a_cons_to_diag(const struct gkyl_wv_eqn* eqn, const double* qin, double* diag)
{
  diag[0] = qin[0];
}

static inline void
~a_source(const struct gkyl_wv_eqn* eqn, const double* qin, double* sout)
{
  sout[0] = 0.0;
}

void
gkyl_~a_free(const struct gkyl_ref_count* ref)
{
  struct gkyl_wv_eqn* base = container_of(ref, struct gkyl_wv_eqn, ref_count);

  if (gkyl_wv_eqn_is_cu_dev(base)) {
    // Free inner on_dev object.
    struct wv_~a *~a = container_of(base->on_dev, struct wv_~a, eqn);
    gkyl_cu_free(~a);
  }

  struct wv_~a *~a = container_of(base, struct wv_~a, eqn);
  gkyl_free(~a);
}

struct gkyl_wv_eqn*
gkyl_wv_~a_new(~a bool use_gpu)
{
  return gkyl_wv_~a_inew(&(struct gkyl_wv_~a_inp) {
      ~a
      .rp_type = WV_~a_RP_LAX,
      .use_gpu = use_gpu,
    }
  );
}

struct gkyl_wv_eqn*
gkyl_wv_~a_inew(const struct gkyl_wv_~a_inp* inp)
{
  struct wv_~a *~a = gkyl_malloc(sizeof(struct wv_~a));

  ~a->eqn.type = GKYL_EQN_~a;
  ~a->eqn.num_equations = 1;
  ~a->eqn.num_diag = 1;

  ~a

  if (inp->rp_type == WV_~a_RP_LAX) {
    ~a->eqn.num_waves = 2;
    ~a->eqn.waves_func = wave_lax_l;
    ~a->eqn.qfluct_func = qfluct_lax_l;
  }

  ~a->eqn.flux_jump = flux_jump;
  ~a->eqn.check_inv_func = check_inv;
  ~a->eqn.max_speed_func = max_speed;
  ~a->eqn.rotate_to_local_func = rot_to_local;
  ~a->eqn.rotate_to_global_func = rot_to_global;
  
  ~a->eqn.wall_bc_func = ~a_wall;
  ~a->eqn.no_slip_bc_func = ~a_no_slip;

  ~a->eqn.cons_to_riem = cons_to_riem;
  ~a->eqn.riem_to_cons = riem_to_cons;

  ~a->eqn.cons_to_diag = ~a_cons_to_diag;

  ~a->eqn.source_func = ~a_source;

  ~a->eqn.flags = 0;
  GKYL_CLEAR_CU_ALLOC(~a->eqn.flags);
  ~a->eqn.ref_count = gkyl_ref_count_init(gkyl_~a_free);
  ~a->eqn.on_dev = &~a->eqn; // On the CPU, the equation object points to itself.

  return &~a->eqn;
}
"
           name
           name
           name
           parameter-sig
           max-speed-local
           name
           parameter-sig
           flux-ui
           name
           name
           name
           name
           name
           parameter-def
           name
           parameter-name
           name
           parameter-name
           name
           parameter-name
           name
           parameter-name
           name
           name
           name
           parameter-def
           name
           parameter-name
           name
           parameter-name
           name
           parameter-name
           name
           parameter-name
           name
           name
           name
           parameter-def
           name
           parameter-name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           parameter-sig
           name
           name
           parameter-field
           (string-upcase name)
           name
           name
           name
           name
           name
           name
           (string-upcase name)
           name
           name
           parameter-field-set
           (string-upcase name)
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           ))
  code)

;; ----------------------------------------------------------------
;; Header for Gkeyll Roe (Finite-Volume) Solver for a 1D Scalar PDE
;; ----------------------------------------------------------------
(define (gkyl-generate-roe-scalar-1d-header pde
                                            #:nx [nx 200]
                                            #:x0 [x0 0.0]
                                            #:x1 [x1 2.0]
                                            #:t-final [t-final 1.0]
                                            #:cfl [cfl 0.95]
                                            #:init-func [init-func `(cond
                                                                      [(< x 1.0) 1.0]
                                                                      [else 0.0])])
 "Generate Gkeyll C header code that solves the 1D scalar PDE specified by `pde` using the Roe finite-volume method.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define parameters (hash-ref pde 'parameters))

  (define parameter-def (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) "; // Additional simulation parameter.")]
                          [else ""]))
  (define parameter-sig (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) ",")]
                          [else ""]))
  (define parameter-comment (cond
                              [(not (empty? parameters)) (string-append "* @param " (convert-expr (list-ref parameters 1)) " Additional simulation parameter.")]
                              [else "*"]))

  (define code
    (format "
#pragma once

#include <gkyl_wv_eqn.h>

// Type of Riemann-solver to use:
enum gkyl_wv_~a_rp {
  WV_~a_RP_ROE = 0, // Default (Roe fluxes).
};

// Input context, packaged as a struct.
struct gkyl_wv_~a_inp {
  ~a

  enum gkyl_wv_~a_rp rp_type; // Type of Riemann-solver to use.
  bool use_gpu; // Whether the wave equation object is on the host (false) or the device (true).
};

/**
* Create a new ~a equations object.
*
~a
* @param use_gpu Whether the wave equation object is on the host (false) or the device (true).
* @return Pointer to the ~a equations object.
*/
struct gkyl_wv_eqn*
gkyl_wv_~a_new(~a bool use_gpu);

/**
* Create a new ~a equations object, from an input context struct.
*
* @param inp Input context struct.
* @return Pointer to the ~a equations object.
*/
struct gkyl_wv_eqn*
gkyl_wv_~a_inew(const struct gkyl_wv_~a_inp* inp);
"
           name
           (string-upcase name)
           name
           parameter-def
           name
           name
           parameter-comment
           name
           name
           parameter-sig
           name
           name
           name
           name
           ))
  code)

;; ------------------------------------------------------------------------
;; Private Header for Gkeyll Roe (Finite-Volume) Solver for a 1D Scalar PDE
;; ------------------------------------------------------------------------
(define (gkyl-generate-roe-scalar-1d-priv-header pde
                                                 #:nx [nx 200]
                                                 #:x0 [x0 0.0]
                                                 #:x1 [x1 2.0]
                                                 #:t-final [t-final 1.0]
                                                 #:cfl [cfl 0.95]
                                                 #:init-func [init-func `(cond
                                                                           [(< x 1.0) 1.0]
                                                                           [else 0.0])])
 "Generate Gkeyll C private header code that solves the 1D scalar PDE specified by `pde` using the Roe finite-volume method.
  - `nx` : Number of spatial cells.
  - `x0`, `x1` : Domain boundaries.
  - `t-final`: Final time.
  - `cfl`: CFL coefficient.
  - `init-func`: Racket expression for the initial condition, e.g. piecewise constant."

  (define name (hash-ref pde 'name))
  (define parameters (hash-ref pde 'parameters))

  (define parameter-def (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) "; // Additional simulation parameter.")]
                          [else ""]))
  (define parameter-sig (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) ",")]
                          [else ""]))
  (define parameter-comment (cond
                              [(not (empty? parameters)) (string-append "* @param " (convert-expr (list-ref parameters 1)) " Additional simulation parameter.")]
                              [else "*"]))

  (define code
    (format "
#pragma once

// Private header, not for direct use in user-facing code.

#include <math.h>
#include <gkyl_array.h>
#include <gkyl_wv_eqn.h>
#include <gkyl_eqn_type.h>
#include <gkyl_range.h>
#include <gkyl_util.h>

struct wv_~a {
  struct gkyl_wv_eqn eqn; // Base equation object.
  ~a
};

/**
* Compute maximum absolute wave speed.
*
~a
* @param q Conserved variable vector.
* @return Maximum absolute wave speed for a given q.
*/
GKYL_CU_D
static inline double
gkyl_~a_max_abs_speed(~a const double* q);

/**
* Compute flux vector. Assumes rotation to local coordinate system.
*
~a
* @param q Conserved variable vector.
* @param flux Flux vector in direction 'dir' (output).
*/
GKYL_CU_D
void
gkyl_~a_flux(~a const double* q, double* flux);

/**
* Compute eigenvalues of the flux Jacobian. Assumes rotation to local coordinate system.
*
~a
* @param q Conserved variable vector.
* @param flux_deriv Flux Jacobian eigenvalues in direction 'dir' (output).
*/
GKYL_CU_D
void
gkyl_~a_flux_deriv(~a const double* q, double* flux_deriv);

/**
* Compute Riemann variables given the conserved variables.
*
* @param eqn Base equation object.
* @param qstate Current state vector.
* @param qin Conserved variable vector (input).
* @param wout Riemann variable vector (output).
*/
GKYL_CU_D
static inline void
cons_to_riem(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* qin, double* wout);

/**
* Compute conserved variables given the Riemann variables.
*
* @param eqn Base equation object.
* @param qstate Current state vector.
* @param win Riemann variable vector (input).
* @param qout Conserved variable vector (output).
*/
GKYL_CU_D
static inline void
riem_to_cons(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* win, double *qout);

/**
* Boundary condition function for applying wall boundary conditions for the ~a equations.
*
* @param eqn Base equation object.
* @param t Current simulation time.
* @param nc Number of boundary cells to which to apply wall boundary conditions.
* @param skin Skin cells in boundary region (from which values are copied).
* @param ghost Ghost cells in boundary region (to which values are copied).
* @param ctx Context to pass to the function.
*/
GKYL_CU_D
static void
~a_wall(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx);

/**
* Boundary condition function for applying no-slip boundary conditions for the ~a equations.
*
* @param eqn Base equation object.
* @param t Current simulation time.
* @param nc Number of boundary cells to which to apply no-slip boundary conditions.
* @param skin Skin cells in boundary region (from which values are copied).
* @param ghost Ghost cells in boundary region (to which values are copied).
* @param ctx Context to pass to the function.
*/
GKYL_CU_D
static void
~a_no_slip(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx);

/**
* Rotate state vector from global to local coordinate frame.
*
* @param eqn Base equation object.
* @param tau1 First tangent vector of the coordinate frame.
* @param tau2 Second tangent vector of the coordinate frame.
* @param norm Normal vector of the coordinate frame.
* @param qglobal State vector in global coordinate frame (input).
* @param qlocal State vector in local coordinate frame (output).
*/
GKYL_CU_D
static inline void
rot_to_local(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qglobal,
  double* GKYL_RESTRICT qlocal);

/**
* Rotate state vector from local to global coordinate frame.
*
* @param eqn Base equation object.
* @param tau1 First tangent vector of the coordinate frame.
* @param tau2 Second tangent vector of the coordinate frame.
* @param norm Normal vector of the coordinate frame.
* @param qlocal State vector in local coordinate frame (input).
* @param qglobal State vector in global coordinate frame (output).
*/
GKYL_CU_D
static inline void
rot_to_global(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qlocal,
  double* GKYL_RESTRICT qglobal);

/**
* Compute waves and speeds using Roe fluxes.
*
* @param eqn Base equation object.
* @param delta Jump across interface to split.
* @param ql Conserved variables on the left of the interface.
* @param qr Conserved variables on the right of the interface.
* @param waves Waves (output).
* @param s Wave speeds (output).
* @return Maximum wave speed.
*/
GKYL_CU_D
static double
wave_roe(const struct gkyl_wv_eqn* eqn, const double* delta, const double* ql, const double* qr, double* waves, double* s);

/**
* Compute fluctuations using Roe fluxes.
*
* @param eqn Base equation object.
* @param ql Conserved variable vector on the left of the interface.
* @param qr Conserved variable vector on the right of the interface.
* @param waves Waves (input).
* @param s Wave speeds (input).
* @param amdq Left-moving fluctuations (output).
* @param apdq Right-moving fluctuations (output).
*/
GKYL_CU_D
static void
qfluct_roe(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, const double* waves, const double* s, double* amdq, double* apdq);

/**
* Compute waves and speeds using Roe fluxes (with potential fallback).
*
* @param eqn Base equation object.
* @param type Type of Riemann-solver flux to use.
* @param delta Jump across interface to split.
* @param ql Conserved variables on the left of the interface.
* @param qr Conserved variables on the right of the interface.
* @param waves Waves (output).
* @param s Wave speeds (output).
* @return Maximum wave speed.
*/
GKYL_CU_D
static double
wave_roe_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* delta, const double* ql, const double* qr, double* waves, double* s);

/**
* Compute fluctuations using Roe fluxes (with potential fallback),
*
* @param eqn Base equation object.
* @param type Type of Riemann-solver flux to use.
* @param ql Conserved variable vector on the left of the interface.
* @param qr Conserved variable vector on the right of the interface.
* @param waves Waves (input).
* @param s Wave speeds (input).
* @param amdq Left-moving fluctuations (output).
* @param apdq Right-moving fluctuations (output).
*/
GKYL_CU_D
static void
qfluct_roe_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* ql, const double* qr, const double* waves, const double* s,
  double* amdq, double* apdq);

/**
* Compute jump in flux given two conserved variable states.
*
* @param eqn Base equation object.
* @param ql Conserved variable vector on the left of the interface (input).
* @param qr Conserved variable vector on the right of the interface (input).
* @param flux_jump Jump in flux vector (output).
* @return Maximum wave speeds for states ql and qr.
*/
GKYL_CU_D
static double
flux_jump(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, double* flux_jump);

/**
* Determine whether invariant domain of the ~a equations is satisfied.
*
* @param eqn Base equation object.
* @param q Conserved variable vector.
* @return Whether the invariant domain is satisfied.
*/
GKYL_CU_D
static bool
check_inv(const struct gkyl_wv_eqn* eqn, const double* q);

/**
* Compute maximum wave speed from a conserved variable vector.
*
* @param eqn Base equation object.
* @param q Conserved variable vector.
* @return Maximum absolute wave speed.
*/
GKYL_CU_D
static double
max_speed(const struct gkyl_wv_eqn* eqn, const double* q);

/**
* Convert conserved variables to diagnostic variables.
*
* @param eqn Base equation object.
* @param qin Conserved variable vector (input).
* @param diag Diagnostic variable vector (output).
*/
GKYL_CU_D
static inline void
~a_cons_to_diag(const struct gkyl_wv_eqn* eqn, const double* qin, double* diag);

/**
* Compute forcing/source term vector from conserved variable vector.
*
* @param eqn Base equation object.
* @param qin Conserved variable vector (input).
* @param sout Forcing/source term vector (output).
*/
GKYL_CU_DH
static inline void
~a_source(const struct gkyl_wv_eqn* eqn, const double* qin, double* sout);

/**
* Free ~a equations object.
*
* @param ref Reference counter for ~a equations.
*/
void gkyl_~a_free(const struct gkyl_ref_count* ref);
"
           name
           parameter-def
           parameter-comment
           name
           parameter-sig
           parameter-comment
           name
           parameter-sig
           parameter-comment
           name
           parameter-sig
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           ))
  code)

;; ----------------------------------------------------------------
;; Source for Gkeyll Roe (Finite-Volume) Solver for a 1D Scalar PDE
;; ----------------------------------------------------------------
(define (gkyl-generate-roe-scalar-1d-source pde
                                            #:nx [nx 200]
                                            #:x0 [x0 0.0]
                                            #:x1 [x1 2.0]
                                            #:t-final [t-final 1.0]
                                            #:cfl [cfl 0.95]
                                            #:init-func [init-func `(cond
                                                                      [(< x 1.0) 1.0]
                                                                      [else 0.0])])
 "Generate Gkeyll C source code that solves the 1D scalar PDE specified by `pde` using the Roe finite-volume method.
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

  (define flux-deriv (symbolic-simp (symbolic-diff flux-expr cons-expr)))

  (define cons-code (convert-expr cons-expr))
  (define flux-code (convert-expr flux-expr))
  (define flux-deriv-code (convert-expr flux-deriv))
  (define max-speed-code (convert-expr max-speed-expr))
  (define init-func-code (convert-expr init-func))

  (define max-speed-local (flux-substitute max-speed-code cons-code "q[0]"))
  (define flux-ui (flux-substitute flux-code cons-code "q[0]"))
  (define flux-deriv-ui (flux-substitute flux-deriv-code cons-code "q[0]"))
  
  (define parameter-def (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) " = "
                                                                    name "->" (convert-expr (list-ref parameters 1)) "; // Additional simulation parameter.")]
                          [else ""]))
  (define parameter-sig (cond
                          [(not (empty? parameters)) (string-append "double " (convert-expr (list-ref parameters 1)) ",")]
                          [else ""]))
  (define parameter-name (cond
                           [(not (empty? parameters)) (string-append (convert-expr (list-ref parameters 1)) ",")]
                           [else ""]))
  (define parameter-field (cond
                            [(not (empty? parameters)) (string-append "." (convert-expr (list-ref parameters 1)) " = "
                                                                      (convert-expr (list-ref parameters 1)) ",")]
                            [else ""]))
  (define parameter-field-set (cond
                                [(not (empty? parameters)) (string-append name "->" (convert-expr (list-ref parameters 1))
                                                                          " = inp->" (convert-expr (list-ref parameters 1)) ";")]
                                [else ""]))

  (define code
    (format "
#include <assert.h>
#include <math.h>

#include <gkyl_alloc.h>
#include <gkyl_alloc_flags_priv.h>
#include <gkyl_wv_~a.h>
#include <gkyl_wv_~a_priv.h>

static inline double
gkyl_~a_max_abs_speed(~a const double* q)
{
  return ~a;
}

void
gkyl_~a_flux(~a const double* q, double* flux)
{
  flux[0] = ~a;
}

void
gkyl_~a_flux_deriv(~a const double* q, double* flux_deriv)
{
  flux_deriv[0] = ~a;
}

static inline void
cons_to_riem(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* qin, double* wout)
{
  // TODO: This should use a proper L matrix.
  wout[0] = qin[0];
}

static inline void
riem_to_cons(const struct gkyl_wv_eqn* eqn, const double* qstate, const double* win, double* qout)
{
  // TODO: This should use a proper L matrix.
  qout[0] = win[0];
}

static void
~a_wall(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx)
{
  ghost[0] = skin[0];
}

static void
~a_no_slip(const struct gkyl_wv_eqn* eqn, double t, int nc, const double* skin, double* GKYL_RESTRICT ghost, void* ctx)
{
  ghost[0] = skin[0];
}

static inline void
rot_to_local(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qglobal,
  double* GKYL_RESTRICT qlocal)
{
  qlocal[0] = qglobal[0];
}

static inline void
rot_to_global(const struct gkyl_wv_eqn* eqn, const double* tau1, const double* tau2, const double* norm, const double* GKYL_RESTRICT qlocal,
  double* GKYL_RESTRICT qglobal)
{
  qglobal[0] = qlocal[0];
}

static double
wave_roe(const struct gkyl_wv_eqn* eqn, const double* delta, const double* ql, const double* qr, double* waves, double* s)
{
  const struct wv_~a *~a = container_of(eqn, struct wv_~a, eqn);
  ~a

  double *fl_deriv = gkyl_malloc(sizeof(double));
  double *fr_deriv = gkyl_malloc(sizeof(double));
  gkyl_~a_flux_deriv(~a ql, fl_deriv);
  gkyl_~a_flux_deriv(~a qr, fr_deriv);

  double a_roe = 0.5 * (fl_deriv[0] + fr_deriv[0]);

  double *w0 = &waves[0];
  w0[0] = delta[0];

  s[0] = a_roe;

  gkyl_free(fl_deriv);
  gkyl_free(fr_deriv);

  return s[0];
}

static void
qfluct_roe(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, const double* waves, const double* s, double* amdq, double* apdq)
{
  const double *w0 = &waves[0];

  if (s[0] < 0.0) {
    amdq[0] = s[0] * w0[0];
    apdq[0] = 0.0;
  }
  else {
    amdq[0] = 0.0;
    apdq[0] = s[0] * w0[0];
  }
}

static double
wave_roe_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* delta, const double* ql, const double* qr, double* waves, double* s)
{
  return wave_roe(eqn, delta, ql, qr, waves, s);
}

static void
qfluct_roe_l(const struct gkyl_wv_eqn* eqn, enum gkyl_wv_flux_type type, const double* ql, const double* qr, const double* waves, const double* s,
  double* amdq, double* apdq)
{
  return qfluct_roe(eqn, ql, qr, waves, s, amdq, apdq);
}

static double
flux_jump(const struct gkyl_wv_eqn* eqn, const double* ql, const double* qr, double* flux_jump)
{
  const struct wv_~a *~a = container_of(eqn, struct wv_~a, eqn);
  ~a

  double *fr = gkyl_malloc(sizeof(double));
  double *fl = gkyl_malloc(sizeof(double));
  gkyl_~a_flux(~a ql, fl);
  gkyl_~a_flux(~a qr, fr);

  flux_jump[0] = fr[0] - fl[0];

  double amaxl = gkyl_~a_max_abs_speed(~a ql);
  double amaxr = gkyl_~a_max_abs_speed(~a qr);
  
  gkyl_free(fr);
  gkyl_free(fl);

  return fmax(amaxl, amaxr);
}

static bool
check_inv(const struct gkyl_wv_eqn* eqn, const double* q)
{
  return true; // All states are assumed to be valid.
}

static double
max_speed(const struct gkyl_wv_eqn* eqn, const double* q)
{
  const struct wv_~a *~a = container_of(eqn, struct wv_~a, eqn);
  ~a

  return gkyl_~a_max_abs_speed(~a q);
}

static inline void
~a_cons_to_diag(const struct gkyl_wv_eqn* eqn, const double* qin, double* diag)
{
  diag[0] = qin[0];
}

static inline void
~a_source(const struct gkyl_wv_eqn* eqn, const double* qin, double* sout)
{
  sout[0] = 0.0;
}

void
gkyl_~a_free(const struct gkyl_ref_count* ref)
{
  struct gkyl_wv_eqn* base = container_of(ref, struct gkyl_wv_eqn, ref_count);

  if (gkyl_wv_eqn_is_cu_dev(base)) {
    // Free inner on_dev object.
    struct wv_~a *~a = container_of(base->on_dev, struct wv_~a, eqn);
    gkyl_cu_free(~a);
  }

  struct wv_~a *~a = container_of(base, struct wv_~a, eqn);
  gkyl_free(~a);
}

struct gkyl_wv_eqn*
gkyl_wv_~a_new(~a bool use_gpu)
{
  return gkyl_wv_~a_inew(&(struct gkyl_wv_~a_inp) {
      ~a
      .rp_type = WV_~a_RP_ROE,
      .use_gpu = use_gpu,
    }
  );
}

struct gkyl_wv_eqn*
gkyl_wv_~a_inew(const struct gkyl_wv_~a_inp* inp)
{
  struct wv_~a *~a = gkyl_malloc(sizeof(struct wv_~a));

  ~a->eqn.type = GKYL_EQN_~a;
  ~a->eqn.num_equations = 1;
  ~a->eqn.num_diag = 1;

  ~a

  if (inp->rp_type == WV_~a_RP_ROE) {
    ~a->eqn.num_waves = 1;
    ~a->eqn.waves_func = wave_roe_l;
    ~a->eqn.qfluct_func = qfluct_roe_l;
  }

  ~a->eqn.flux_jump = flux_jump;
  ~a->eqn.check_inv_func = check_inv;
  ~a->eqn.max_speed_func = max_speed;
  ~a->eqn.rotate_to_local_func = rot_to_local;
  ~a->eqn.rotate_to_global_func = rot_to_global;
  
  ~a->eqn.wall_bc_func = ~a_wall;
  ~a->eqn.no_slip_bc_func = ~a_no_slip;

  ~a->eqn.cons_to_riem = cons_to_riem;
  ~a->eqn.riem_to_cons = riem_to_cons;

  ~a->eqn.cons_to_diag = ~a_cons_to_diag;

  ~a->eqn.source_func = ~a_source;

  ~a->eqn.flags = 0;
  GKYL_CLEAR_CU_ALLOC(~a->eqn.flags);
  ~a->eqn.ref_count = gkyl_ref_count_init(gkyl_~a_free);
  ~a->eqn.on_dev = &~a->eqn; // On the CPU, the equation object points to itself.

  return &~a->eqn;
}
"
           name
           name
           name
           parameter-sig
           max-speed-local
           name
           parameter-sig
           flux-ui
           name
           parameter-sig
           flux-deriv-ui
           name
           name
           ;; Begins here.
           name
           name
           name
           parameter-def
           name
           parameter-name
           name
           parameter-name
           ;; Ends here
           name
           name
           name
           parameter-def
           name
           parameter-name
           name
           parameter-name
           name
           parameter-name
           name
           parameter-name
           name
           name
           name
           parameter-def
           name
           parameter-name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           parameter-sig
           name
           name
           parameter-field
           (string-upcase name)
           name
           name
           name
           name
           name
           name
           (string-upcase name)
           name
           name
           parameter-field-set
           (string-upcase name)
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           name
           ))
  code)