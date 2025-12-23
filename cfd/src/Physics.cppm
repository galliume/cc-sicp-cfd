module;

#include <cassert>

export module Physics;

import std;

import BoundaryConditions;
import Defines;
import Field;
import Fluid;
import Mesh;
import Schemes;

using namespace cfd;

export namespace Physics
{
  template<typename T>
  concept hasBCCallOperator = requires(
    T t,
    double const F, double const D) {
    { t(F, D) };
  };

  template <std::floating_point T>
  void apply_diffusion_operator(
    std::mdspan<T, std::dextents<std::size_t, 2>> grid,
    Mesh const & mesh,
    Field const & velocity,
    Fluid::Properties const & fluid_props,
    Schemes::Types const & scheme)
  {
    assert(grid.extent(1) == 3
          && "TDMA Matrix must be packed tridiagonal (N x 3).");

    assert(grid.extent(0) >= 3
           && "Grid too small for diffusion (min 3 points needed)");

    std::size_t const N { grid.extent(0) };

    std::visit([&](auto&& s) {
      for(std::size_t i { 1 }; i < N - 1; ++i) {
        double const u { velocity[i] };
        double const F { fluid_props.rho * u };
        double const D { fluid_props.gamma / mesh.dx() };

        auto const result { s(F, D) };

        grid[i, CFD_LOWER] = T(result[CFD_LOWER]);
        grid[i, CFD_DIAG] =  T(result[CFD_DIAG]);
        grid[i, CFD_UPPER] = T(result[CFD_UPPER]);
      }
    }, scheme);
  }

  template <std::floating_point T>
  void apply_boundary_conditions(
    std::mdspan<T, std::dextents<std::size_t, 2>> grid,
    BC::Types & left_boundary_condition,
    BC::Types & right_boundary_condition
  )
  {
    assert(grid.extent(1) == 3
          && "TDMA Matrix must be packed tridiagonal (N x 3).");

    assert(grid.extent(0) >= 3
          && "Grid too small for diffusion (min 3 points needed)");

    std::size_t const N { grid.extent(0) };

    std::visit([&](auto&& bc) {
        using T_bc = std::decay_t<decltype(bc)>;
        if constexpr (std::is_same_v<T_bc, BC::Dirichlet>) {
            grid[0, CFD_DIAG] = 1.0;
            grid[0, CFD_UPPER] = 0.0;
        } else if constexpr (std::is_same_v<T_bc, BC::Neumann>) {
            grid[0, CFD_DIAG] = 1.0;
            grid[0, CFD_UPPER] = -1.0;
        }
    }, left_boundary_condition);

    std::visit([&](auto&& bc) {
        using T_bc = std::decay_t<decltype(bc)>;
        std::size_t end { N - 1 };
        if constexpr (std::is_same_v<T_bc, BC::Dirichlet>) {
            grid[end, CFD_DIAG] = 1.0;
            grid[end, CFD_LOWER] = 0.0;
        } else if constexpr (std::is_same_v<T_bc, BC::Neumann>) {
            grid[end, CFD_DIAG] = 1.0;
            grid[end, CFD_LOWER] = -1.0;
        }
    }, right_boundary_condition);
  }
}
