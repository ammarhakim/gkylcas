#include <gkyl_rad_gyrokinetic_kernels.h> 
GKYL_CU_DH double funcName(const double *w, const double *dxv, const double *bmag_inv, const double *nI, const double *vnu, const double *f, double* GKYL_RESTRICT out) 
{ 
  // w[5]: cell-center coordinates. 
  // dxv[5]: cell spacing. 
  // bmag_inv: 1/(magnetic field magnitude). 
  // ion density 
  // v*nu(v) field dg representation. 
  // f: input distribution function.
  // out: incremented output 

  double rdv2[2]; 
  rdv2[0] = 2.0/dxv[3]; 
  rdv2[1] = 2.0/dxv[4]; 

