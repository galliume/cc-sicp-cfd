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

int main(int, char**)
{
  constexpr Index const N { 10000 };
  constexpr Meters const L { 1.0 };
  constexpr Scalar const T_left { 100.0 };
  //constexpr Scalar const G_right { 0.0 };

  Mesh mesh(N, L);
  Field T("Temperature", mesh, 0.0);

  //N rows * 3 columns (left / center / right diagonals instead of full matrices)
  std::vector<double> matrix_data(mesh.n_cells() * 3, 0.0);
  auto A { std::mdspan(matrix_data.data(), mesh.n_cells(), 3) };

  Schemes::Types const scheme { Schemes::CDS() };
  Field const velocity("Velocity", mesh, 0.1);
  Fluid::Properties const air_props(1.204, 0.025);

  Physics::apply_diffusion_operator(A, mesh, velocity, air_props, scheme);

  BC::Types dirichletBCL { BC::Dirichlet(T_left) };
  //BC::Types neumannBC { BC::Neumann(G_right) };
  BC::Types dirichletBCR { BC::Dirichlet(0.0) };

  Physics::apply_boundary_conditions(A, dirichletBCL, dirichletBCR);

  std::vector<double> b(mesh.n_cells(), 0.0);

  std::visit([&](auto&& bc) {
    b[0] = bc.value;
  }, dirichletBCL);

  std::visit([&](auto&& bc) {
    b[mesh.n_cells() - 1] = bc.value;
  }, dirichletBCR);

  for (std::size_t i{0}; i < A.extent(0); ++i) {
    std::cout << std::format("{:.2e} {:.2e} {:.2e}\n", A[i, 0], A[i, 1], A[i, 2]);
  }
  std::cout << "Solving the linear system...\n";
  auto solution { Solver::solve_tdma(A, b) };

  for(auto const cell : mesh) {
    T[cell.index()] = solution[cell.index()];
    std::cout << std::format("T(x={:.3f}) = {:.2f}\n", cell.x(), T[cell.index()]);
  }

  T.to_csv("./final_state.csv");

  return 0;
}
