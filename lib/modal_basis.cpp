#include <iostream>
#include <cassert>

#include "serendip_mono.h"
#include <modal_basis.h>

// Basis function monomial list for each dimension: mo_list[ndim].ev[polyOrder]
static struct { GiNaC::lst (*ev[4])(const std::vector<GiNaC::symbol>&); } mo_list[] = {
  { NULL, NULL, NULL, NULL }, // No 0D basis functions
  { serendip_1x_p0, serendip_1x_p1, serendip_1x_p2, serendip_1x_p3 },
  { serendip_2x_p0, serendip_2x_p1, serendip_2x_p2, serendip_2x_p3 },
  { serendip_3x_p0, serendip_3x_p1, serendip_3x_p2, serendip_3x_p3 },
  { serendip_4x_p0, serendip_4x_p1, serendip_4x_p2, serendip_4x_p3 },
  { serendip_5x_p0, serendip_5x_p1, serendip_5x_p2, serendip_5x_p3 },
  { serendip_6x_p0, serendip_6x_p1, serendip_6x_p2, NULL },
};

Gkyl::ModalBasis::ModalBasis(int ndim, const std::vector<GiNaC::symbol>& invars, int polyOrder)
: ndim(ndim), polyOrder(polyOrder)
{
  assert(ndim<=6 && polyOrder<=3);
  assert(mo_list[ndim].ev[polyOrder] != NULL);

  for (int d=0; d<ndim; ++d) vars.push_back(invars[d]);
  bc = gsOrthoNorm(mo_list[ndim].ev[polyOrder](vars));
}

GiNaC::lst
Gkyl::ModalBasis::diffBasis(int n) const
{
  GiNaC::lst db;
  for (auto bidx = bc.begin(); bidx != bc.end(); ++bidx)
    db.append( GiNaC::diff(*bidx, vars[n]) );
  return db;
}

GiNaC::ex
Gkyl::ModalBasis::innerProd(const GiNaC::ex &f1, const GiNaC::ex &f2) const
{
  GiNaC::ex out = f1*f2;
  for (int i=0; i<ndim; ++i)
    out = GiNaC::integral(vars[i], -1, 1, out);
  return out.eval_integ();
}

GiNaC::ex
Gkyl::ModalBasis::norm(const GiNaC::ex &f) const
{
  return GiNaC::sqrt(innerProd(f, f));
}

GiNaC::ex
Gkyl::ModalBasis::proj(const GiNaC::ex &u, const GiNaC::ex &v) const
{
  return innerProd(u,v)/innerProd(u,u)*u;
}

GiNaC::lst
Gkyl::ModalBasis::gsOrthoNorm(const GiNaC::lst& vec) const
{
  GiNaC::lst orthoVec { vec[0]/norm(vec[0]) };

  if (vec.nops() > 1) {
    auto vitr = vec.begin(); ++vitr;
    for ( ; vitr != vec.end(); ++vitr) {
      GiNaC::ex pj;
      for (auto jitr = orthoVec.begin(); jitr != orthoVec.end(); ++jitr)
        pj += proj(*jitr, *vitr);
      GiNaC::ex v = *vitr - pj;
      orthoVec.append(v/norm(v));
    }
  }
  return orthoVec;
}

GiNaC::ex
Gkyl::ModalBasis::expand(const GiNaC::symbol& f) const
{
  GiNaC::ex fh;
  int i = 0;
  for (auto bidx = bc.begin() ; bidx != bc.end(); ++bidx, ++i)
    fh += (*bidx)*GiNaC::indexed(f, GiNaC::idx(i,1));
  return fh;
}

GiNaC::lst
Gkyl::ModalBasis::calcInnerProdList(const GiNaC::lst &lst, const GiNaC::ex &f) const
{
  GiNaC::lst out;
  for (auto lidx = lst.begin(); lidx != lst.end(); ++lidx)
    out.append( innerProd(*lidx, f) );
  return out;
}
  
