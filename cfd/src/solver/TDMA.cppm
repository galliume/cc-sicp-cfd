module;
#include <cassert>
#include <cstddef>
#include <concepts>
#include <vector>

// #inclue <mdspan>
#include <mdspan/mdspan.hpp>

export module TDMA;

export namespace Solver
{
  /**
   * https://www.cfd-online.com/Wiki/Tridiagonal_matrix_algorithm_-_TDMA_(Thomas_algorithm)
   */
  template <std::floating_point T>
  std::vector<T> solve_tdma(
    Kokkos::mdspan<T, Kokkos::dextents<size_t, 2>> const grid,
    std::vector<T> const & rhs)
  {
    assert(grid.extent(0) == grid.extent(1)
           && "TDMA Matrix must be square.");

    assert(rhs.size() == grid.extent(0)
           && "RHS vector size must match matrix dimension.");

    auto const N { grid.extent(0) };
    auto solution { rhs };

    std::vector<T> grid_data(grid.data_handle(), grid.data_handle() + grid.mapping().required_span_size());
    auto grid_ { Kokkos::mdspan(grid_data.data(), N, N) };

    for (size_t k { 1 }; k < N; ++k) {
      auto const m { grid_[k, k - 1] / grid_[k - 1, k - 1] };
      grid_[k, k] = grid_[k, k] - m * grid_[k - 1, k];
      solution[k] = solution[k] - m * solution[k - 1];
    }

    solution[N - 1] = solution[N - 1] / grid_[N - 1, N - 1];

    for (size_t i { 0 }; i < N - 1; ++i) {
      size_t k { (N - 2) - i };
      solution[k] = (solution[k] - grid_[k, k + 1] * solution[k + 1]) / grid_[k, k];
    }

    return solution;
  }
}
