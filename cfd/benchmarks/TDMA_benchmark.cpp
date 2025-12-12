#include <benchmark/benchmark.h>

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

static void BM_TDMASolverCDSScheme(benchmark::State& state) {
  size_t const N { static_cast<size_t>(state.range(0)) };

  constexpr Meters L { 1.0 };
  constexpr Scalar T_left { 100.0 };
  constexpr Scalar T_right { 0.0 };

  constexpr double rho { 1.0 };
  constexpr double gamma { 0.1 };

  Mesh mesh(N, L);

  std::vector<double> matrix_data(N * 3, 0.0);
  auto A { std::mdspan(matrix_data.data(), N, 3) };

  Schemes::Types const scheme { Schemes::CDS() };
  Field const velocity("Velocity", mesh, 10.0);
  Fluid::Properties fluid_props(rho, gamma);
  std::vector<double> b(mesh.n_cells(), 0.0);

  Physics::apply_diffusion_operator(A, mesh, velocity, fluid_props, scheme);

  BC::Types bc_left  { BC::Dirichlet(T_left) };
  BC::Types bc_right { BC::Dirichlet(T_right) };

  Physics::apply_boundary_conditions(A, bc_left, bc_right);

  std::visit([&](auto&& bc) {
    b[0] = bc.value;
  }, bc_left);

  std::visit([&](auto&& bc) {
    b[mesh.n_cells() - 1] = bc.value;
  }, bc_right);

  std::vector<double> master_matrix = matrix_data;

  for (auto _ : state) {
    state.PauseTiming();
    std::copy(master_matrix.begin(), master_matrix.end(), matrix_data.begin());
    state.ResumeTiming();

    auto solution = Solver::solve_tdma(A, b);
    benchmark::DoNotOptimize(solution.data());
    benchmark::ClobberMemory();
  }
}

static void BM_TDMASolverUDSScheme(benchmark::State& state) {
  size_t const N { static_cast<size_t>(state.range(0)) };

  constexpr Meters L { 1.0 };
  constexpr Scalar T_left { 100.0 };
  constexpr Scalar T_right { 0.0 };

  constexpr double rho { 1.0 };
  constexpr double gamma { 0.1 };

  Mesh mesh(N, L);

  std::vector<double> matrix_data(N * 3, 0.0);
  auto A { std::mdspan(matrix_data.data(), N, 3) };

  Schemes::Types const scheme { Schemes::UDS() };
  Field const velocity("Velocity", mesh, 1000.0);
  Fluid::Properties const air_props(rho, gamma);
  std::vector<double> b(mesh.n_cells(), 0.0);

  Physics::apply_diffusion_operator(A, mesh, velocity, air_props, scheme);

  BC::Types bc_left  { BC::Dirichlet(T_left) };
  BC::Types bc_right { BC::Dirichlet(T_right) };

  Physics::apply_boundary_conditions(A, bc_left, bc_right);
  
  std::visit([&](auto&& bc) {
    b[0] = bc.value;
  }, bc_left);

  std::visit([&](auto&& bc) {
    b[mesh.n_cells() - 1] = bc.value;
  }, bc_right);

  std::vector<double> master_matrix = matrix_data;

  for (auto _ : state) {
    state.PauseTiming();
    std::copy(master_matrix.begin(), master_matrix.end(), matrix_data.begin());
    state.ResumeTiming();

    auto solution = Solver::solve_tdma(A, b);
    benchmark::DoNotOptimize(solution.data());
    benchmark::ClobberMemory();
  }
}

BENCHMARK(BM_TDMASolverUDSScheme)->RangeMultiplier(10)->Range(1000, 10000000);
BENCHMARK(BM_TDMASolverCDSScheme)->RangeMultiplier(10)->Range(1000, 10000000);

BENCHMARK_MAIN();
