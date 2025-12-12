module;

#include <cassert>

export module BoundaryConditions;

import Defines;

import std;

using namespace cfd;

export namespace BC
{
  struct Dirichlet
  {
    Dirichlet(Scalar const _value)
    : value(_value)
    {
      assert(value >= 0.0);
    }
    Scalar value { 100.0 };
  };
  
  struct Neumann
  {
    Neumann(Scalar const _value)
    : value(_value)
    {
      assert(value >= 0.0);
    }
    Scalar value { 0.0 };
  };

  using Types = std::variant<Dirichlet, Neumann>;
}
