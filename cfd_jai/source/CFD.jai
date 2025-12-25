#import "Basic";
#import "Math";
#import "File";

BOUNDARY_CONDITIONS :: #import "boundaryConditions";
FIELD :: #import "field";
MESH :: #import "mesh";//find a better name : MESH.Mesh...
PHYSICS :: #import "physics";
SCHEMES :: #import "schemes";
SOLVER :: #import "TDMA";

main :: () {

    N : u64 : 10000000;//grid size NxN

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

    //grids : [] float64;
    rhs_size : s64 : cast(s64) N;
    grid_size : s64 : rhs_size * 3;

    rhs := NewArray(rhs_size, float64);
    grids := NewArray(grid_size, float64);

    mesh : MESH.Mesh;
    mesh.n_cells = N;
    mesh.length = 1.0; // 1 meters
    mesh.dx = mesh.length / mesh.n_cells;
    mesh.x_centers = NewArray(rhs_size, float64);

    for 0..mesh.n_cells-1 {
        mesh.x_centers[it] = mesh.dx / 2.0 + it * mesh.dx;
    }

    PHYSICS.apply_diffusion_operator(grids, mesh, V, fluidProps, SCHEMES.scheme_cds);
    PHYSICS.apply_boundary_conditions(grids, rhs, T_left, T_right);

    solution := SOLVER.solve_tdma(grids, rhs);

    //file, success :=  file_open("cfd.csv", for_writing=true, keep_existing_content=true);
    //if !success {
    //    print("Could not open file cfd.csv for writing.\n");
    //    return;
    //}
//
    //// advance the file pointer to ensure we append to the file instead of overwrite!
    //advance :=  file_length(file);
    //file_set_position(file, advance);
//
    //success =  file_write(*file, "T[],T\n");
    //if !success {
    //    print("Could not write to file temp_dir/file1.txt.\n");
    //    return;
    //}
    //for 0..solution.count-1 {
    //    //print("T[%] = %\n", mesh.x_centers[it], solution[it]);
    //    // like above, we are able to pass in a string, a pointer and a length, or a String_Builder struct.
    //    success =  file_write(*file, BLOCK_OF_TEXT);
    //    if !success {
    //        print("Could not write to file temp_dir/file1.txt.\n");
    //        return;
    //    }
    //}
    //file_close(*file);
}

