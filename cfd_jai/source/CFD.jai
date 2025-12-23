#import "Basic";
#import "Math";

BOUNDARY_CONDITIONS :: #import "boundaryConditions";
FIELD :: #import "field";
MESH :: #import "mesh";//find a better name : MESH.Mesh...
PHYSICS :: #import "physics";
SCHEMES :: #import "schemes";
SOLVER :: #import "TDMA";

main :: () {

    N : u64 : 1000000;//grid size NxN

    fluidProps : FIELD.FluidProperties;
    fluidProps.rho = 1.204;
    fluidProps.gamma = 0.025;

    T_left : BOUNDARY_CONDITIONS.DirichletBoundaryCondition;
    T_left.value = 100.0;

    T_right : BOUNDARY_CONDITIONS.DirichletBoundaryCondition;
    T_right.value = 0.0;

    T : FIELD.Field;
    T.name = "Temperature";
    for 1..N array_add(*T.values, 0.0);

    V : FIELD.Field;
    V.name = "Velocity";
    for 1..N array_add(*V.values, 0.1);

    mesh : MESH.Mesh;
    mesh.n_cells = N;
    mesh.length = 1.0; // 1 meters
    mesh.dx = mesh.length / mesh.n_cells;
    for 0..mesh.n_cells {
        array_add(*mesh.x_centers, mesh.dx / 2.0 + it * mesh.dx);
    }

    grids : [N * 3] float;
    rhs : [N] float;

    PHYSICS.apply_diffusion_operator(grids, mesh, V, fluidProps, SCHEMES.scheme_cds);
    PHYSICS.apply_boundary_conditions(grids, rhs, T_left, T_right);

    solution := SOLVER.solve_tdma(grids, rhs);

    for 0..solution.count-1 {
        print("T[%] = %\n", mesh.x_centers[it], solution[it]);
    }
}

