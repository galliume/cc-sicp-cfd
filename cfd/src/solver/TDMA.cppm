module;
#include <cassert>
#include <cstddef>
#include <concepts>
#include <vector>

// #inclue <mdspan>
#include <mdspan/mdspan.hpp>

import Defines;

using namespace cfd;

export module TDMA;

export namespace Solver
{
  /**
   * https://www.cfd-online.com/Wiki/TriCFD_DIAGonal_matrix_algorithm_-_TDMA_(Thomas_algorithm)
   */
  template <std::floating_point T>
  std::vector<T> solve_tdma(
    Kokkos::mdspan<T, Kokkos::dextents<size_t, 2>> const grid,
    std::vector<T> const & rhs)
  {
    assert(grid.extent(1) == 3
          && "TDMA Matrix must be packed tridiagonal (N x 3).");

    assert(rhs.size() == grid.extent(0)
           && "RHS vector size must match matrix dimension.");

    auto const N { grid.extent(0) };
    auto solution { rhs };

    std::vector<T> grid_data(grid.data_handle(), grid.data_handle() + grid.mapping().required_span_size());
    auto grid_ { Kokkos::mdspan(grid_data.data(), N, 3) };

    for (size_t k { 1 }; k < N; ++k) {
      auto const m { grid_[k, CFD_LOWER] / grid_[k - 1, CFD_DIAG] };
      grid_[k, CFD_DIAG] = grid_[k, CFD_DIAG] - m * grid_[k - 1, CFD_UPPER];
      solution[k] = solution[k] - m * solution[k - 1];
    }

    solution[N - 1] = solution[N - 1] / grid_[N - 1, CFD_DIAG];

    for (size_t i { 0 }; i < N - 1; ++i) {
      size_t k { (N - 2) - i };
      solution[k] = (solution[k] - grid_[k, CFD_UPPER] * solution[k + 1]) / grid_[k, CFD_DIAG];
    }

    return solution;
  }
}
