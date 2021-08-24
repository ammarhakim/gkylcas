#include "tensor_mono.h"

GiNaC::lst
tensor_2x_p2(const std::vector<GiNaC::symbol>& vars)
{
  GiNaC::symbol x = vars[0], y = vars[1];
  auto x2 = x*x;
  auto y2 = y*y;
  GiNaC::lst l { 1,x,y,x*y,x*x,y*y,x*x*y,x*y*y,x*x*y*y };
  return l;
}

GiNaC::lst
tensor_3x_p2(const std::vector<GiNaC::symbol>& vars)
{
  GiNaC::symbol x = vars[0], y = vars[1], z = vars[2];
  auto x2 = x*x;
  auto y2 = y*y;
  auto z2 = z*z;
  GiNaC::lst l { 1,x,y,z,x*y,x*z,y*z,x*x,y*y,z*z,x*y*z,x*x*y,x*y*y,x*x*z,y*y*z,x*z*z,y*z*z,x*x*y*z,x*y*y*z,x*y*z*z };
  return l;
}

GiNaC::lst
tensor_4x_p2(const std::vector<GiNaC::symbol>& vars)
{
  GiNaC::symbol x = vars[0], y = vars[1], z = vars[2], u = vars[3];
  auto x2 = x*x;
  auto y2 = y*y;
  auto z2 = z*z;
  auto u2 = u*u;
  
  GiNaC::lst l { 1,x,y,z,u,x*y,x*z,y*z,u*x,u*y,u*z,x*x,y*y,z*z,u*u,x*y*z,u*x*y,u*x*z,u*y*z,x*x*y,x*y*y,x*x*z,y*y*z,x*z*z,y*z*z,u*x*x,u*y*y,u*z*z,u*u*x,u*u*y,u*u*z,u*x*y*z,x*x*y*z,x*y*y*z,x*y*z*z,u*x*x*y,u*x*y*y,u*x*x*z,u*y*y*z,u*x*z*z,u*y*z*z,u*u*x*y,u*u*x*z,u*u*y*z,u*x*x*y*z,u*x*y*y*z,u*x*y*z*z,u*u*x*y*z };
  return l;
}

GiNaC::lst
tensor_5x_p2(const std::vector<GiNaC::symbol>& vars)
{
  GiNaC::symbol x = vars[0], y = vars[1], z = vars[2], u = vars[3], v = vars[4];
  auto x2 = x*x;
  auto y2 = y*y;
  auto z2 = z*z;
  auto u2 = u*u;
  auto v2 = v*v;
  
  GiNaC::lst l { 1,x,y,z,u,v,x*y,x*z,y*z,u*x,u*y,u*z,v*x,v*y,v*z,u*v,x*x,y*y,z*z,u*u,v*v,x*y*z,u*x*y,u*x*z,u*y*z,v*x*y,v*x*z,v*y*z,u*v*x,u*v*y,u*v*z,x*x*y,x*y*y,x*x*z,y*y*z,x*z*z,y*z*z,u*x*x,u*y*y,u*z*z,u*u*x,u*u*y,u*u*z,v*x*x,v*y*y,v*z*z,u*u*v,v*v*x,v*v*y,v*v*z,u*v*v,u*x*y*z,v*x*y*z,u*v*x*y,u*v*x*z,u*v*y*z,x*x*y*z,x*y*y*z,x*y*z*z,u*x*x*y,u*x*y*y,u*x*x*z,u*y*y*z,u*x*z*z,u*y*z*z,u*u*x*y,u*u*x*z,u*u*y*z,v*x*x*y,v*x*y*y,v*x*x*z,v*y*y*z,v*x*z*z,v*y*z*z,u*v*x*x,u*v*y*y,u*v*z*z,u*u*v*x,u*u*v*y,u*u*v*z,v*v*x*y,v*v*x*z,v*v*y*z,u*v*v*x,u*v*v*y,u*v*v*z,u*v*x*y*z,u*x*x*y*z,u*x*y*y*z,u*x*y*z*z,u*u*x*y*z,v*x*x*y*z,v*x*y*y*z,v*x*y*z*z,u*v*x*x*y,u*v*x*y*y,u*v*x*x*z,u*v*y*y*z,u*v*x*z*z,u*v*y*z*z,u*u*v*x*y,u*u*v*x*z,u*u*v*y*z,v*v*x*y*z,u*v*v*x*y,u*v*v*x*z,u*v*v*y*z,u*v*x*x*y*z,u*v*x*y*y*z,u*v*x*y*z*z,u*u*v*x*y*z,u*v*v*x*y*z };
  return l;
}
