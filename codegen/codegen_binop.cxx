#include <modal_basis.h>
#include <iostream>
#include <fstream>

using namespace GiNaC;

static void
gen_ser_mul_op(std::ostream& fout, const Gkyl::ModalBasis& basis)
{
  int ndim = basis.get_ndim(), polyOrder = basis.get_polyOrder();
  lst bc = basis.get_basis();

  // function opening
  //fout << "GKYL_CU_DH" << std::endl;
  fout << "static void" << std::endl;
  fout << "binop_mul_" << ndim << "d_ser_" << "p" << polyOrder
       << "(const double *f, const double *g, double *fg )" << std::endl;
  fout << "{" << std::endl;  

  symbol f("f"), g("g");
  auto fh = basis.expand(f), gh = basis.expand(g);
  auto mul = basis.calcInnerProdList(bc, fh*gh);

  for (int i=0; i<basis.get_numbasis(); ++i)
    fout << "  fg[" << i << "] = " << csrc << mul[i].expand().evalf() << ";" << std::endl;

  // close function
  fout << "}" << std::endl << std::endl;  
}

int
main(int argc, char **argv)
{
  int dims[] = { 1, 2, 3, 4, 5, 6 };
  int max_order[] = { 3, 3, 3, 3, 3, 2 };

  symbol z0("z0"), z1("z1"), z2("z2"), z3("z3"), z4("z4"), z5("z5");
  std::vector<symbol> vars { z0, z1, z2, z3, z4, z5 };

  std::ofstream mul_file("binop_mul_ser.c", std::ofstream::out);

  for (int d=0; d<3; ++d) {
    int dim = dims[d];
    for (int p=0; p<=max_order[d]; ++p) {
      Gkyl::ModalBasis mbasis(dim, vars, p);
      std::cout << dim << "dp" << p << " ";
      
      // generate multiply method
      gen_ser_mul_op(mul_file, mbasis);
    }
    std::cout << std::endl;
  }
  
  return 1;
}
