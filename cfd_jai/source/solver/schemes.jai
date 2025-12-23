#import "Basic";
#import "Math";

scheme_uds :: (
    F: float,
    D: float
) -> [] float
{
    results : [3] float;

    a_W := D + max(F, 0.0);
    a_E := D + max(-F, 0.0);
    a_P := a_W + a_E;

    results[0] = -a_W;
    results[1] =  a_P;
    results[2] = -a_E;

    return results;
}

scheme_cds :: (
    F: float,
    D: float
) -> [] float
{
    results : [3] float;

    a_W := D + F / 2.0;
    a_E := D - F / 2.0;
    a_P := a_W + a_E;

    results[0] = -a_W;
    results[1]  =  a_P;
    results[2] = -a_E;

    return results;
}

#scope_module;
