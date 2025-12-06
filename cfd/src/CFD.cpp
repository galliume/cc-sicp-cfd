#include <cmath>
#include <format>
#include <iostream>
#include <numbers>

import Mesh;
import Field;

using namespace cfd;

int main(int, char**)
{
  std::cout << "Initializing Simulation...\n";

  Index const N { 0 };
  Meters const L { 1.0 }; 
  Mesh grid(N, L);

  Scalar const initial_T { 0.0 };
  Field T("Temperature", grid, initial_T);

  for(Index i { 0 }; i < T.size(); i++) {
      Coordinate const x { grid.x(i) };
      T[i] = std::sin(std::numbers::pi * x);
  }

  T.to_string();
  T.to_csv("init_state.csv");

  return 0;
}
