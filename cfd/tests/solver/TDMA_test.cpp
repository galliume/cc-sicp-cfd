#include <cmath>
#include <chrono>
#include <gtest/gtest.h>
#include <vector>

#include <mdspan/mdspan.hpp>

import TDMA;
import Physics;

namespace {
  void ExpectVectorsNear(const std::vector<double>& a, const std::vector<double>& b, double tolerance) {
    ASSERT_EQ(a.size(), b.size());
    for (size_t i { 0 }; i < a.size(); ++i) {
      EXPECT_NEAR(a[i], b[i], tolerance);
    }
  }
}

TEST(TDMATest, Solves4x4SystemCorrectly) {
  constexpr size_t N { 4 };
  std::vector<double> matrix_data(N * 3, 0.0);
  auto A { Kokkos::mdspan(matrix_data.data(), N, 3) };

  Physics::apply_diffusion_operator(A);
  Physics::apply_boundary_conditions(A);

  std::vector<double> rhs{ 0.0, 0.0, 0.0, 100.0 };
  auto solution { Solver::solve_tdma(A, rhs) };

  EXPECT_EQ(solution[0], 0.0);
  EXPECT_EQ(solution[N-1], 100.0);

  std::vector<double> expected_solution{ 0.0, 33.333333, 66.666667, 100.0 };
  ExpectVectorsNear(solution, expected_solution, 1e-6);

  auto solutionBis { Solver::solve_tdma(A, rhs) };
  ExpectVectorsNear(solutionBis, expected_solution, 1e-6);
}

TEST(TDMATest, SolvesLargeSystemLinearGradient) {
  auto start = std::chrono::high_resolution_clock::now();

  constexpr size_t N { 10000000 };
  std::vector<double> matrix_data(N * 3, 0.0);
  auto A { Kokkos::mdspan(matrix_data.data(), N, 3) };

  Physics::apply_diffusion_operator(A);
  Physics::apply_boundary_conditions(A);

  std::vector<double> rhs(N, 0.0);
  rhs[0] = 0.0;
  rhs[N - 1] = 100.0;

  auto solution { Solver::solve_tdma(A, rhs) };

  auto end = std::chrono::high_resolution_clock::now();
  auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

  EXPECT_LT(duration, 500) << "TDMA took " << duration << "ms (expected < 500ms)";
}
