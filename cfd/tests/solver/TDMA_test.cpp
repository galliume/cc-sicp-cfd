#include <gtest/gtest.h>

import std;

import BoundaryConditions;
import Defines;
import Field;
import Fluid;
import Mesh;
import Physics;
import Schemes;
import TDMA;

using namespace cfd;

// Analytical Solution for 1D Convection-Diffusion (Dirichlet-Dirichlet)
// T(0) = T_left, T(L) = T_right
double exact_solution(double x, double L, double Pe, double T_left, double T_right) {
  if (std::abs(Pe) < 1e-9) {
    // Pure Diffusion Limit (Linear)
    return T_left + (T_right - T_left) * (x / L);
  }
  // Exact exponential solution
  return T_left + (T_right - T_left) * ( (std::exp(Pe * x / L) - 1.0) / (std::exp(Pe) - 1.0) );
}

TEST(ValidationTest, ConvectionDiffusionAnalytical) {
  // Test Parameters
  constexpr Index N { 100 }; // Good resolution for validation
  constexpr Meters L { 1.0 };
  constexpr Scalar T_left { 100.0 };
  constexpr Scalar T_right { 0.0 };

  // Fluid Properties
  constexpr double rho { 1.0 };
  constexpr double gamma { 0.1 };

  // Test Cases: Different Peclet Numbers (Pe = rho * u * L / gamma)
  // We achieve this by varying velocity 'u'
  struct TestCase {
    double target_Pe;
    double tolerance_uds; // UDS is 1st order, so error is larger
    double tolerance_cds; // CDS is 2nd order, but oscillates at high Pe
  };

  std::vector<TestCase> cases = {
    {10.0, 6.0, 1.0},   // Pe = 10 (Moderate convection)
    {50.0, 25.0, 50.0}, // Pe = 50 (High convection) - UDS smears heavily here
    {100.0, 45.0, 100.0} // Pe = 100 (Very high) - UDS error is large but stable
  };

  for (const auto& test : cases) {
    // Calculate required velocity to hit target Pe
    // Pe = (rho * u * L) / gamma  =>  u = (Pe * gamma) / (rho * L)
    double u { (test.target_Pe * gamma) / (rho * L) };

    //std::cout << "\nTesting Pe = " << test.target_Pe << " (u = " << u << ")\n";

    // 1. Setup Simulation
    Mesh mesh(N, L);
    std::vector<double> matrix_data(N * 3, 0.0);
    auto A { std::mdspan(matrix_data.data(), N, 3) };
    std::vector<double> b(mesh.n_cells(), 0.0);

    Field velocity("Velocity", mesh, u);
    Fluid::Properties fluid_props(rho, gamma);

    // 2. Setup Boundary Conditions (Dirichlet-Dirichlet)
    BC::Types bc_left  { BC::Dirichlet(T_left) };
    BC::Types bc_right { BC::Dirichlet(T_right) };

    // ---------------------------------------------------------
    // RUN 1: UPWIND SCHEME (UDS)
    // ---------------------------------------------------------
    Physics::apply_diffusion_operator(A, mesh, velocity, fluid_props, Schemes::UDS{});
    Physics::apply_boundary_conditions(A, bc_left, bc_right);

    std::visit([&](auto&& bc) {
      b[0] = bc.value;
    }, bc_left);

    std::visit([&](auto&& bc) {
      b[mesh.n_cells() - 1] = bc.value;
    }, bc_right);
    
    // Solve (using a copy because solve destroys A)
    std::vector<double> A_data_copy = matrix_data; 
    auto solution_uds { Solver::solve_tdma(
      std::mdspan(A_data_copy.data(), N, 3), b
    ) };

    // Validate UDS
    double max_error_uds { 0.0 };
    for(size_t i = 0; i < N; ++i) {
      double exact = exact_solution(mesh.x(i), L, test.target_Pe, T_left, T_right);
      double error = std::abs(solution_uds[i] - exact);
      max_error_uds = std::max(max_error_uds, error);
    }
    std::cout << "  [UDS] Max Error: " << max_error_uds << "\n";
    EXPECT_LT(max_error_uds, test.tolerance_uds) << "UDS failed at Pe=" << test.target_Pe;

    // ---------------------------------------------------------
    // RUN 2: CENTRAL DIFFERENCE SCHEME (CDS)
    // ---------------------------------------------------------
    // Reset matrix
    std::fill(matrix_data.begin(), matrix_data.end(), 0.0);
    std::fill(b.begin(), b.end(), 0.0);

    Physics::apply_diffusion_operator(A, mesh, velocity, fluid_props, Schemes::CDS{});
    Physics::apply_boundary_conditions(A, bc_left, bc_right);

    std::visit([&](auto&& bc) {
      b[0] = bc.value;
    }, bc_left);

    std::visit([&](auto&& bc) {
      b[mesh.n_cells() - 1] = bc.value;
    }, bc_right);

    A_data_copy = matrix_data;
    auto solution_cds { Solver::solve_tdma(
      std::mdspan(A_data_copy.data(), N, 3), b
    ) };

    // Check for Oscillations (CDS will violate bounds at high Pe)
    bool has_undershoot { false };
    bool has_overshoot = { false };
    for(double val : solution_cds) {
      if (val < -0.01) has_undershoot = true; // T_right is 0
      if (val > 100.01) has_overshoot = true; // T_left is 100
    }

    if (test.target_Pe > 2.0) {
      // At High Pe, CDS *should* be unstable physically.
      // We might expect it to fail the accuracy check, or produce wiggles.
      // This print confirms we see the instability.
      if (has_overshoot || has_undershoot) {
        std::cout << "  [CDS] Oscillations detected (Expected behavior at High Pe)\n";
      }
    }
  }
}