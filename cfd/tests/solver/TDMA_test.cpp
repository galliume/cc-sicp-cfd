#include <gtest/gtest.h>
#include <vector>
#include <cmath>

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
  std::vector<double> matrix_data(N * N, 0.0);
  auto A { Kokkos::mdspan(matrix_data.data(), N, N) };

  Physics::apply_diffusion_operator(A);
  Physics::apply_boundary_conditions(A);

  std::vector<double> rhs{ 0.0, 0.0, 0.0, 100.0 };
  auto solution { Solver::solve_tdma(A, rhs) };

  std::vector<double> expected_solution{ 0.0, 33.333333, 66.666667, 100.0 };
  ExpectVectorsNear(solution, expected_solution, 1e-6);
}
