MESH :: #import "mesh";

FluidProperties :: struct {
    rho : float64;
    gamma  : float64;
};


Field :: struct {
    name: string;
    mesh: MESH.Mesh;
    values : [..] float64;
};

#scope_module;
