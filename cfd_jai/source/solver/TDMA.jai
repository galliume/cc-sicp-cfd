#import "Basic";
#import "Math";

solve_tdma :: (
    grids : [] float,
    rhs : [] float
) -> [] float
{
    solution := rhs;
    N := rhs.count;

    index : int = 3;
    rhs_index := 1;

    while rhs_index < N-1 {
        previous_y_offset := index - 3 + 1;
        previous_z_offset := index - 3 + 2;

        x_offset := index;
        y_offset := index + 1;
        z_offset := index + 2;

        m := grids[x_offset] / grids[previous_y_offset];
        grids[y_offset] -= m * grids[previous_z_offset];

        solution[rhs_index] -= m * solution[rhs_index - 1];

        index += 3;
        rhs_index += 1;
    }

    last_row_b := (N - 1) * 3 + 1;
    solution[N - 1] /= grids[last_row_b];

    for 0..N-2 {
        row := (N - 2) - it;
        solution[row] = (solution[row] - grids[row * 3 + 2] * solution[row + 1]) / grids[row * 3 + 1];
    }
    return solution;
}

#scope_module;
