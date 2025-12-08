module;

#include <cassert>
#include <concepts>

#include <mdspan/mdspan.hpp>

export module Physics;

export namespace Physics
{
  template <std::floating_point T>
  void apply_diffusion_operator(Kokkos::mdspan<T, Kokkos::dextents<size_t, 2>> grid)
  {
    assert(grid.extent(0) == grid.extent(1)
           && "TDMA Matrix must be square.");

    assert(grid.extent(0) >= 3
           && "Grid too small for diffusion (min 3 points needed)");

    size_t const N { grid.extent(0) };

    for(size_t i { 1 }; i < N - 1; ++i) {
      grid[i, i - 1] = T(-1.0);
      grid[i, i] =  T(2.0);
      grid[i, i + 1] = T(-1.0);
    }
  }

  template <std::floating_point T>
  void apply_boundary_conditions(
    Kokkos::mdspan<T, Kokkos::dextents<size_t, 2>> grid
  )
  {
    assert(grid.extent(0) == grid.extent(1)
          && "TDMA Matrix must be square.");

    assert(grid.extent(0) >= 3
          && "Grid too small for diffusion (min 3 points needed)");

    size_t const N { grid.extent(0) };

    grid[0, 0] = T(1.0);
    grid[0, 1] = T(0.0);
    grid[N - 1, N - 1] = T(1.0);
    grid[N - 1, N - 2] = T(0.0);
  }
}
