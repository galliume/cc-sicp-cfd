#include <benchmark/benchmark.h>

import TDMA;
import Physics;

import std;

static void BM_TDMASolver(benchmark::State& state) {

  size_t const N { static_cast<size_t>(state.range(0)) };

  std::vector<double> rhs(N, 0.0);
  rhs[0] = 0.0;
  rhs[N - 1] = 100.0;

  for (auto _ : state) {
    state.PauseTiming();

    std::vector<double> matrix_data(N * 3, 0.0);
    auto A { std::mdspan(matrix_data.data(), N, 3) };
    Physics::apply_diffusion_operator(A);
    Physics::apply_boundary_conditions(A);
    state.ResumeTiming();

    auto solution = Solver::solve_tdma(A, rhs);
    benchmark::DoNotOptimize(solution.data());
    benchmark::ClobberMemory();
  }
}

BENCHMARK(BM_TDMASolver)->RangeMultiplier(10)->Range(1000, 10000000);
