module;

#include <cassert>

export module TDMA;

import Defines;

import std;

using namespace cfd;

export namespace Solver
{
  /**
   * https://www.cfd-online.com/Wiki/TriCFD_DIAGonal_matrix_algorithm_-_TDMA_(Thomas_algorithm)
   */
  template <std::floating_point T> [[nodiscard]]
  std::vector<T> solve_tdma(
    std::mdspan<T, std::dextents<std::size_t, 2>> grid,
    std::vector<T> const & rhs)
  {
    assert(grid.extent(1) == 3
          && "TDMA Matrix must be packed tridiagonal (N x 3).");

    assert(rhs.size() == grid.extent(0)
           && "RHS vector size must match matrix dimension.");

    auto const N { grid.extent(0) };
    auto solution { rhs };

    for (std::size_t k { 1 }; k < N; ++k) {
      auto const m { grid[k, CFD_LOWER] / grid[k - 1, CFD_DIAG] };
      grid[k, CFD_DIAG] = grid[k, CFD_DIAG] - m * grid[k - 1, CFD_UPPER];
      solution[k] = solution[k] - m * solution[k - 1];
    }

    solution[N - 1] = solution[N - 1] / grid[N - 1, CFD_DIAG];

    for (std::size_t i { 0 }; i < N - 1; ++i) {
      std::size_t k { (N - 2) - i };
      solution[k] = (solution[k] - grid[k, CFD_UPPER] * solution[k + 1]) / grid[k, CFD_DIAG];
    }

    return solution;
  }
}
