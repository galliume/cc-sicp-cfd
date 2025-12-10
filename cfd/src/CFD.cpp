import std;

import Defines;
import Field;
import Mesh;
import Physics;
import TDMA;

using namespace cfd;

int main(int, char**)
{
  Index const N { 40 };
  Meters const L { 1.0 };
  Scalar const T_left { 0.0 };
  Scalar const T_right { 100.0 };

  Mesh mesh(N, L);
  Field T("Temperature", mesh, 0.0);

  //N rows * 3 columns (left / center / right diagonals instead of full matrices)
  std::vector<double> matrix_data(mesh.n_cells() * 3, 0.0);
  auto A { std::mdspan(matrix_data.data(), mesh.n_cells(), 3) };

  Physics::apply_diffusion_operator(A);
  Physics::apply_boundary_conditions(A);

  std::vector<double> b(mesh.n_cells(), 0.0);
  b[0] = T_left;
  b[mesh.n_cells() - 1] = T_right;

  std::cout << "Solving the linear system...\n";
  auto solution { Solver::solve_tdma(A, b) };

  for(auto const cell : mesh) {
    T[cell.index()] = solution[cell.index()];
    std::cout << std::format("T(x={:.3f}) = {:.2f}\n", cell.x(), T[cell.index()]);
  }

  T.to_csv("./final_state.csv");

  return 0;
}
