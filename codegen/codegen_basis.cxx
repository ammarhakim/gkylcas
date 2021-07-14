#include <modal_basis.h>
#include <iostream>
#include <fstream>

using namespace GiNaC;

// Generates the function that evaluates the basis
// functions. Generated function signature:
//
// static void f(const double *z, double *b)
//
// Restrict keyword and CUDA attributes are also added
//
void gen_ser_basis_func(std::ostream& fout, const Gkyl::ModalBasis& basis)
{
  int ndim = basis.get_ndim(), polyOrder = basis.get_polyOrder();

  // function opening
  fout << "static void" << std::endl;
  fout << "eval_" << ndim << "d_" << "p" << polyOrder
       << "(const double *z, double *b ) {" << std::endl;

  // declarations
  for (int d=0; d<ndim; ++d)
    fout << "  const double z" << d << " = " << "z[" << d << "];" << std::endl;

  lst bc = basis.basis();
  // expressions to compute basis functions
  for (int i=0; i<basis.numBasis(); ++i)
    fout << "  b[" << i << "] = " << csrc << bc[i].expand().evalf() << ";" << std::endl;

  // close function
  fout << "}" << std::endl;
}

int
main(int argc, char **argv)
{
  int dims[] = { 1, 2, 3, 4, 5, 6};
  //int max_order[] = { 3, 3, 3, 3, 3, 2};
  int max_order[] = { 1, 1, 1, 1, 1, 0};

  symbol z0("z0"), z1("z1"), z2("z2"), z3("z3"), z4("z4"), z5("z5");
  std::vector<symbol> vars { z0, z1, z2, z3, z4, z5 };

  for (int d=0; d<6; ++d) {
    int dim = dims[d];
    for (int p=0; p<=max_order[d]; ++p) {
      Gkyl::ModalBasis mbasis(dim, vars, p);
      gen_ser_basis_func(std::cout, mbasis);
    }
  }
  
  return 1;
}
