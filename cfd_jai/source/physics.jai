#import "Basic";
#import "Math";

BOUNDARY_CONDITIONS :: #import "boundaryConditions";
FIELD :: #import "field";
MESH :: #import "mesh";
SCHEMES :: #import "schemes";

apply_diffusion_operator :: (
    grids : [] float64,
    mesh : MESH.Mesh,
    velocity : FIELD.Field,
    props : FIELD.FluidProperties,
    scheme : (F: float64, D: float64) -> [] float64)
{
    result : [] float64;
    index := 0;
    field_index := 0;

    while index < grids.count - 1 {
        //make constants
        u : float64 = velocity.values[field_index];
        F : float64 = props.rho * u;
        D : float64 = props.gamma / mesh.dx;

        result = scheme(F, D);
        grids[index] = result[0];
        grids[index + 1] = result[1];
        grids[index + 2] = result[2];

        index += 3;
        field_index += 1;
    }
}

//metaprog for boundary conditions
apply_boundary_conditions :: (
    grids : [] float64,
    rhs : [] float64,
    left_boundary_condition : BOUNDARY_CONDITIONS.DirichletBoundaryCondition,
    right_boundary_condition : BOUNDARY_CONDITIONS.DirichletBoundaryCondition)
{
    //todo add Neumann boundary conditions

    //left bc
    grids[0] = 0.0;
    grids[1] = 1.0;
    grids[2] = 0.0;
    rhs[0] = left_boundary_condition.value;

    //right bc
    grids[grids.count-1] = 0.0;
    grids[grids.count-2] = 1.0;
    grids[grids.count-3] = 0.0;
    rhs[rhs.count - 1] = right_boundary_condition.value;
}

#scope_module;
