module;

#include <cassert>

export module Fluid;

import std;

import Defines;

using namespace cfd;

export namespace Fluid
{
  struct Properties
  {
    Properties(Scalar const _rho, Scalar const _gamma)
    : rho(_rho)
    , gamma(_gamma)
    {
      assert(rho > 0.0);
      assert(gamma > 0.0);
    }
    Scalar rho;
    Scalar gamma;
  };
}
