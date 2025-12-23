MESH :: #import "mesh";

FluidProperties :: struct {
    rho : float;
    gamma  : float;
};


Field :: struct {
    name: string;
    mesh: MESH.Mesh;
    values : [..] float;
};

#scope_module;
