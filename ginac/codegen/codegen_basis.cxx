#include <modal_basis.h>
#include <iostream>
#include <fstream>
#include <gkyl_util.h>

using namespace GiNaC;

// Generates function that evaluates the basis functions. Generated
// function signature:
//
// static void foo(const double *z, double *b)
//
// Restrict keyword and CUDA attributes are also added
//
// fh: header file
// fc: C file
//
static void
gen_ser_eval(std::ostream& fh, std::ostream& fc, const Gkyl::ModalBasis& basis)
{
  int ndim = basis.get_ndim(), polyOrder = basis.get_polyOrder();

  // function declaration
  fh << "GKYL_CU_DH void eval_" << ndim << "d_ser_" << "p" << polyOrder
     << "(const double *z, double *b);" << std::endl;  

  // function definition
  fc << "GKYL_CU_DH" << std::endl;
  fc << "void" << std::endl;
  fc << "eval_" << ndim << "d_ser_" << "p" << polyOrder
       << "(const double *z, double *b )" << std::endl;
  fc << "{" << std::endl;

  // local declarations
  if (polyOrder > 0)
    for (int d=0; d<ndim; ++d)
      fc << "  const double z" << d << " = " << "z[" << d << "];" << std::endl;

  lst bc = basis.get_basis();
  // expressions to compute basis functions
  for (int i=0; i<basis.get_numbasis(); ++i)
    fc << "  b[" << i << "] = " << csrc << bc[i].expand().evalf() << ";" << std::endl;

  // close function
  fc << "}" << std::endl << std::endl;
}

// Generates function that flips sign of basis expansion. Generated
// function signature:
//
// static void foo(int dir, const double *fin, double *fout)
//
// Restrict keyword and CUDA attributes are also added
//
// fh: header file
// fc: C file
//
static void
gen_ser_flip_sign(std::ostream& fh, std::ostream& fc, const Gkyl::ModalBasis& basis)
{
  int ndim = basis.get_ndim(), polyOrder = basis.get_polyOrder();

  // function declarations
  fh << "GKYL_CU_DH void flip_sign_" << ndim << "d_ser_" << "p" << polyOrder
       << "(int dir, const double *f, double *fout );" << std::endl;  

  // function definition
  fc << "GKYL_CU_DH" << std::endl;
  fc << "void" << std::endl;
  fc << "flip_sign_" << ndim << "d_ser_" << "p" << polyOrder
       << "(int dir, const double *f, double *fout )" << std::endl;
  fc << "{" << std::endl;
  
  lst vars = basis.get_vars(), bc = basis.get_basis();

  for (int d=0; d<ndim; ++d) {
    exmap m; m[vars[d]] = -vars[d];
    auto bcflip = bc.subs(m);
    lst signs;
    fc << "  if (dir == " << d << ") {" << std::endl;
    for (int i=0; i<basis.get_numbasis(); ++i) {
      auto sign = bcflip[i]/bc[i];
      fc << "    fout[" << i << "] = " << sign << "*" << "f[" << i <<  "];" << std::endl;
    }
    fc << "  }" << std::endl;
  }

  // close function
  fc << "}" << std::endl << std::endl;  
}

int
main(int argc, char **argv)
{
  // compute time-stamp
  char buff[70];
  time_t t = time(NULL);
  struct tm curr_tm = *localtime(&t);
  strftime(buff, sizeof buff, "%c", &curr_tm);
  
  int dims[] = { 1, 2, 3, 4, 5, 6 };
  int max_order[] = { 3, 3, 3, 3, 3, 2 };

  symbol z0("z0"), z1("z1"), z2("z2"), z3("z3"), z4("z4"), z5("z5");
  std::vector<symbol> vars { z0, z1, z2, z3, z4, z5 };

  std::ofstream header("kernels/basis/gkyl_basis_ser_kernels.h", std::ofstream::out);
  header << "// " << buff << std::endl;
  header << "#pragma once" << std::endl;
  header << "#include <gkyl_util.h>" << std::endl;
  header << "EXTERN_C_BEG" << std::endl;
  
  std::ofstream eval_file("kernels/basis/basis_eval_ser.c", std::ofstream::out);
  std::ofstream flip_file("kernels/basis/basis_flip_sign_ser.c", std::ofstream::out);

  eval_file << "// " << buff << std::endl;
  eval_file << "#include <gkyl_basis_ser_kernels.h>" << std::endl;

  flip_file << "// " << buff << std::endl;
  flip_file << "#include <gkyl_basis_ser_kernels.h>" << std::endl;

  struct timespec tstart = gkyl_wall_clock();

  for (int d=0; d<6; ++d) {
    int dim = dims[d];
    for (int p=0; p<=max_order[d]; ++p) {
      std::cout << dim << "dp" << p << " ";      
      Gkyl::ModalBasis mbasis(Gkyl::MODAL_SER, dim, vars, p);
      
      // generate eval method
      gen_ser_eval(header, eval_file, mbasis);
      // generate flip_sign method
      gen_ser_flip_sign(header, flip_file, mbasis);
    }
    std::cout << std::endl;
  }

  header << "EXTERN_C_END" << std::endl;

  double tm = gkyl_time_diff_now_sec(tstart);
  std::cout << "Took " << tm << " seconds" << std::endl;
  
  return 1;
}
