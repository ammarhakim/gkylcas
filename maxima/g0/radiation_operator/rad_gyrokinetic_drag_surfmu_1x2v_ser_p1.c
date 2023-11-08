#include <gkyl_rad_gyrokinetic_kernels.h> 
GKYL_CU_DH double rad_gyrokinetic_drag_surfmu_1x2v_ser_p1(const double *w, const double *dxv, const double *bmag_inv, const double *nI, const double *vnu, const double *fl, const double *fc, const double *fr, double* GKYL_RESTRICT out) 
{ 
  // w[3]:    cell-center coordinates. 
  // dxv[3]:  cell spacing. 
  // bmag_inv: 1/(magnetic field magnitude). 
  // nI:       ion density 
  // vnu:      v*nu(v) field dg represenation 
  // fl/fc/fr: distribution function in cells 
  // out: incremented distribution function in cell 

  double rdv2 = 2.0/dxv[2]; 

  out[0] += sqrt(GKYL_ELECTRON_MASS)*sqrt(bmag_inv)*(0.3535533905932737*(nI[1]*vnu[1]+nI[0]*vnu[0])*f_re-0.3535533905932737*(nI[1]*vnu[1]+nI[0]*vnu[0])*f_ce)*sqrt(dxv[vidx1[2]]*mu+2.0*w[vidx1[2]]); 
  out[1] += sqrt(GKYL_ELECTRON_MASS)*sqrt(bmag_inv)*(0.3535533905932737*(nI[0]*vnu[1]+vnu[0]*nI[1])*f_re-0.3535533905932737*(nI[0]*vnu[1]+vnu[0]*nI[1])*f_ce)*sqrt(dxv[vidx1[2]]*mu+2.0*w[vidx1[2]]); 
  out[2] += sqrt(GKYL_ELECTRON_MASS)*sqrt(bmag_inv)*(0.3535533905932737*(nI[1]*vnu[3]+nI[0]*vnu[2])*f_re-0.3535533905932737*(nI[1]*vnu[3]+nI[0]*vnu[2])*f_ce)*sqrt(dxv[vidx1[2]]*mu+2.0*w[vidx1[2]]); 
  out[3] += sqrt(GKYL_ELECTRON_MASS)*sqrt(bmag_inv)*(0.3535533905932737*(nI[0]*vnu[3]+nI[1]*vnu[2])*f_re-0.3535533905932737*(nI[0]*vnu[3]+nI[1]*vnu[2])*f_ce)*sqrt(dxv[vidx1[2]]*mu+2.0*w[vidx1[2]]); 
  out[4] += sqrt(GKYL_ELECTRON_MASS)*sqrt(bmag_inv)*(0.3535533905932737*(nI[1]*vnu[5]+nI[0]*vnu[4])*f_re-0.3535533905932737*(nI[1]*vnu[5]+nI[0]*vnu[4])*f_ce)*sqrt(dxv[vidx1[2]]*mu+2.0*w[vidx1[2]]); 
  out[5] += sqrt(GKYL_ELECTRON_MASS)*sqrt(bmag_inv)*(0.3535533905932737*(nI[0]*vnu[5]+nI[1]*vnu[4])*f_re-0.3535533905932737*(nI[0]*vnu[5]+nI[1]*vnu[4])*f_ce)*sqrt(dxv[vidx1[2]]*mu+2.0*w[vidx1[2]]); 
  return 0.;

} 
