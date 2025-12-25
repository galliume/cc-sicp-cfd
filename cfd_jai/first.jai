OUTPUT_EXECUTABLE_NAME := "cfd_jai";

#run {
    set_build_options_dc(.{do_output = false});

    args := get_build_options().compile_time_command_line;

    target_x64     := array_find(args, "-x64");
    run     := array_find(args, "-run");

    cpu_target := CPU;
    os_target  := OS;
    if target_x64      cpu_target = .X64;

    target_triple_with_sdk: string;

    log("Compiling for % %", os_target, cpu_target);

    build_output_directory:     string;
    resources_output_directroy: string;

    w := compiler_create_workspace("Target workspace");
    options := get_build_options(w);
    set_working_directory(#filepath);

    source_path := join(#filepath, "source");
    solver_path := join(#filepath, "source/solver");

    modules_paths: [..] string;
    array_add(*modules_paths, source_path);
    array_add(*modules_paths, solver_path);
    array_add(*modules_paths, ..options.import_path);

    options.import_path = modules_paths;
    options.output_executable_name = "target";

    copy_commonly_propagated_fields(get_build_options(), *options);
    options.cpu_target = cpu_target;
    options.os_target  = os_target;
    options.output_executable_name = OUTPUT_EXECUTABLE_NAME;

    options.output_path = build_output_directory;

    set_build_options(options, w);

    compiler_begin_intercept(w);
    add_build_file("source/benchmark.jai", w);

    while true {
        message := compiler_wait_for_message();
        if message.kind == {
            case .IMPORT;
              import := cast(*Message_Import) message;
              log("Import: '%'\n", import.module_name);
              if import.module_name == "boundaryConditions"  boundaryConditions = import;
              if import.module_name == "field"  field = import;
              if import.module_name == "mesh"  mesh = import;
              if import.module_name == "physics"  physics = import;
              if import.module_name == "schemes"  schemes = import;
              if import.module_name == "solver"  solver = import;

            case .PHASE;
                phase_message := cast(*Message_Phase) message;

            case .COMPLETE;
                mc := cast(*Message_Complete) message;
                if mc.error_code != .NONE {
                    exit(1);
                }

                break;
        }
    }
    compiler_end_intercept(w);
    print("\n");

    if os_target == {
        case;
            if run Autorun.run_build_result(w);
    }
};

boundaryConditions: *Message_Import;
field: *Message_Import;
mesh: *Message_Import;
physics: *Message_Import;
schemes: *Message_Import;
solver: *Message_Import;

#import "Basic";
#import "Compiler";
#import "File";
#import "File_Utilities";
#import "String";
Autorun :: #import "Autorun";

// For getenv?
#if OS == .WINDOWS {
    #import "Windows";
} else {
    #import "POSIX";
}
