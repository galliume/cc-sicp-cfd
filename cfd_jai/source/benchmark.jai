#import "Basic";
#import "Math";
#import "Pool";

BOUNDARY_CONDITIONS :: #import "boundaryConditions";
FIELD :: #import "field";
MESH :: #import "mesh";//find a better name : MESH.Mesh...
PHYSICS :: #import "physics";
SCHEMES :: #import "schemes";
SOLVER :: #import "TDMA";

main :: () {

        elements : [5] u64;
        elements[0] = 1000;
        elements[1] = 10000;
        elements[2] = 100000;
        elements[3] = 1000000;
        elements[4] = 10000000;

        iterations : [5] s64;
        iterations[0] = 45000;
        iterations[1] = 4500;
        iterations[2] = 450;
        iterations[3] = 50;
        iterations[4] = 3;

        for 0..4 {
            ARRAY_SIZE := elements[it];
            BENCHMARK_ITERATIONS := iterations[it];
            log("\nCFD 1D convection-diffusion CDS scheme benchmark: % elements, % iterations", ARRAY_SIZE, BENCHMARK_ITERATIONS);

            min, max, average := simple_benchmark(BENCHMARK_ITERATIONS, #code { },
            #code {
                    N : u64 = `ARRAY_SIZE;//grid size NxN

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
                    rhs_size : s64 = cast(s64) N;
                    grid_size : s64 = rhs_size * 3;

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
                }
            );
            log("\nArray:\nMin: %us\nMax: %us\nAverage: %us", to_microseconds(min), to_microseconds(max), to_microseconds(average));
            log("\nFactor: %", to_microseconds(min)/cast(float)to_microseconds(min));
    }
}

simple_benchmark :: (iterations: int, prepare: Code, run: Code) -> Apollo_Time, Apollo_Time, Apollo_Time #expand {
    pool: Pool;
    set_allocators(*pool);

    min: Apollo_Time;
    max: Apollo_Time;
    average: Apollo_Time;
    for it: 0..iterations-1 {
        push_allocator(pool_allocator_proc, *pool);
        defer reset(*pool);

        #insert,scope() prepare;

        start := current_time_monotonic();
        #insert,scope() run;
        end := current_time_monotonic();
        duration := end-start;
        if it == 0 || duration < min min = duration;
        if duration > max max = duration;
        average += duration;
    }
    average /= iterations;
    return min, max, average;
}
