module;

#include <cassert>

export module Schemes;

import Defines;

import std;

using namespace cfd;

export namespace Schemes
{
  struct UDS
  {
    std::array<Scalar, 3> operator()(Scalar F, Scalar D) const
    {
      std::array<Scalar, 3> result;

      Scalar a_W = D + std::max(F, 0.0);
      Scalar a_E = D + std::max(-F, 0.0);
      Scalar a_P = a_W + a_E;

      result[CFD_LOWER] = -a_W;
      result[CFD_DIAG]  =  a_P;
      result[CFD_UPPER] = -a_E;

      return result;
    }
  };

  struct CDS
  {
    std::array<Scalar, 3> operator()(Scalar F, Scalar D) const
    {
      std::array<Scalar, 3> result;

      Scalar a_W = D + F / 2.0;
      Scalar a_E = D - F / 2.0;
      Scalar a_P = a_W + a_E;

      result[CFD_LOWER] = -a_W;
      result[CFD_DIAG]  =  a_P;
      result[CFD_UPPER] = -a_E;

      return result;
    }
  };

  using Types = std::variant<UDS, CDS>;
}
