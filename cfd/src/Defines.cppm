module;

#include <cstddef>

export module Defines;

export namespace cfd
{
  using Coordinate = double;
  using Index = std::size_t;
  using Meters = double;
  using Scalar = double;

  inline constexpr size_t CFD_LOWER { 0 }; // i-1
  inline constexpr size_t CFD_DIAG { 1 }; // i
  inline constexpr size_t CFD_UPPER { 2 }; // i+1
}
