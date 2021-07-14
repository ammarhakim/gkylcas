#pragma once

#include <vector>
#include <ginac/ginac.h>

namespace Gkyl {
  class ModalBasis {
  public:
    /* Construct new modal basis object */
    ModalBasis(int ndim, const std::vector<GiNaC::symbol>& vars, int polyOrder);
    
    /* Dimensions and polyorder */
    int get_ndim() const { return ndim; }
    int get_polyOrder() const { return polyOrder; }
    
    /* Get number of basis functions */
    int numBasis() const { return bc.nops(); }
    /* Get list of basis functions */
    GiNaC::lst basis() const { return bc; }

    /* Get derivative of basis functions wrt to n-th indep. var */
    GiNaC::lst diffBasis(int n) const;

    /* Generate indexed expansion with symbol 'f' and basis */
    GiNaC::ex expand(const GiNaC::symbol& f) const;

    /* Compute inner product of f1 and f2 */
    GiNaC::ex innerProd(const GiNaC::ex &f1, const GiNaC::ex &f2) const;

    /* Calculate inner product with list of expressions */
    GiNaC::lst calcInnerProdList(const GiNaC::lst &lst, const GiNaC::ex &f) const;

  private:
    int ndim, polyOrder;
    GiNaC::lst bc; // orthonormal basis set
    std::vector<GiNaC::symbol> vars; // Variable list

    /* Compute L2-norm of f */
    GiNaC::ex norm(const GiNaC::ex &f) const;
    /* Compute projection of u on v */
    GiNaC::ex proj(const GiNaC::ex &u, const GiNaC::ex &v) const;

    /* Orthonormalize list of monomials */
    GiNaC::lst gsOrthoNorm(const GiNaC::lst& vec) const;
  };
}
