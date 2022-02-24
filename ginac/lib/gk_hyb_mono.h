#pragma once

#include <ginac/ginac.h>

// 1x1v
GiNaC::lst gk_hyb_2x_p1(const std::vector<GiNaC::symbol> &vars);
// 1x2v
GiNaC::lst gk_hyb_3x_p1(const std::vector<GiNaC::symbol> &vars);
// 2x2v
GiNaC::lst gk_hyb_4x_p1(const std::vector<GiNaC::symbol> &vars);
// 3x2v
GiNaC::lst gk_hyb_5x_p1(const std::vector<GiNaC::symbol>& vars);
