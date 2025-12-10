module;

#include <cassert>

export module Physics;

import Defines;

import std;

using namespace cfd;

export namespace Physics
{
  template <std::floating_point T>
  void apply_diffusion_operator(std::mdspan<T, std::dextents<std::size_t, 2>> grid)
  {
    assert(grid.extent(1) == 3
          && "TDMA Matrix must be packed tridiagonal (N x 3).");

    assert(grid.extent(0) >= 3
           && "Grid too small for diffusion (min 3 points needed)");

    std::size_t const N { grid.extent(0) };

    for(std::size_t i { 1 }; i < N - 1; ++i) {
      grid[i, CFD_LOWER] = T(-1.0);
      grid[i, CFD_DIAG] =  T(2.0);
      grid[i, CFD_UPPER] = T(-1.0);
    }
  }

  template <std::floating_point T>
  void apply_boundary_conditions(
    std::mdspan<T, std::dextents<std::size_t, 2>> grid
  )
  {
    assert(grid.extent(1) == 3
          && "TDMA Matrix must be packed tridiagonal (N x 3).");

    assert(grid.extent(0) >= 3
          && "Grid too small for diffusion (min 3 points needed)");

    std::size_t const N { grid.extent(0) };

    grid[0, CFD_DIAG] = T(1.0);
    grid[0, CFD_UPPER] = T(0.0);
    grid[0, CFD_LOWER] = T(0.0);

    grid[N - 1, CFD_DIAG] = T(1.0);
    grid[N - 1, CFD_UPPER] = T(0.0);
    grid[N - 1, CFD_LOWER] = T(0.0);
  }
}
