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

  // 1. Define the physical problem
  Index const N { 40 };
  Meters const L { 1.0 };
  Scalar const T_left { 0.0 };
  Scalar const T_right { 100.0 };

  // 2. Create the mesh and field
  Mesh mesh(N, L);
  Field T("Temperature", mesh, 0.0); // Initial guess

  // 3. Assemble the linear system (A*T = b) for d2T/dx2 = 0
  // Allocate memory for the matrix A
  std::vector<double> matrix_data(N * N, 0.0);
  auto A { Kokkos::mdspan(matrix_data.data(), N, N) };

  // Fill the interior points with the diffusion operator
  Physics::apply_diffusion_operator(A);
  // Set the boundary condition equations
  Physics::apply_boundary_conditions(A);

  // Assemble the right-hand-side vector b
  std::vector<double> b(N, 0.0);
  b[0] = T_left;
  b[N - 1] = T_right;

  // 4. Solve the linear system
  std::cout << "Solving the linear system...\n";
  auto solution { Solver::solve_tdma(A, b) };

  // 5. Update the field and write results
  for(Index i { 0 }; i < N; ++i) {
    T[i] = solution[i];
    std::cout << std::format("T[{}] = {}\n", i, T[i]);
  }

  T.to_csv("./final_state.csv");

  return 0;
}
