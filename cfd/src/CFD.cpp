#include <format>
#include <iostream>
#include <vector>

//#include <mdspan>
#include <mdspan/mdspan.hpp>

import Mesh;
import Field;
import TDMA;
import Physics;

using namespace cfd;

int main(int, char**)
{
  std::cout << "Initializing Simulation...\n";

  Index const N { 40 };
  Meters const L { 1.0 };
  Scalar const T_left { 0.0 };
  Scalar const T_right { 100.0 };

  Mesh mesh(N, L);
  Field T("Temperature", mesh, 0.0);

  std::vector<double> matrix_data(N * N, 0.0);
  auto A { Kokkos::mdspan(matrix_data.data(), N, N) };

  Physics::apply_diffusion_operator(A);
  Physics::apply_boundary_conditions(A);

  std::vector<double> b(N, 0.0);
  b[0] = T_left;
  b[N - 1] = T_right;

  std::cout << "Solving the linear system...\n";
  auto solution { Solver::solve_tdma(A, b) };

  for(Index i { 0 }; i < N; ++i) {
    T[i] = solution[i];
    std::cout << std::format("T[{}] = {}\n", i, T[i]);
  }

  T.to_csv("./final_state.csv");

  return 0;
}
