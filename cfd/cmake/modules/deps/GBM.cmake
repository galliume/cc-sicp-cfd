message(NOTICE "Fetching google/benchmark from https://github.com/google/benchmark ...")

set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "" FORCE)

FetchContent_Declare(
  fetch_benchmark
  GIT_REPOSITORY https://github.com/google/benchmark.git
  GIT_TAG        ${GBM_TAG}
  GIT_SHALLOW ${FETCH_GIT_SHALLOW}
  GIT_PROGRESS ${FETCH_GIT_PROGRESS}
  UPDATE_DISCONNECTED ${FETCH_UPDATE_DISCONNECTED}
)

if(TARGET benchmark::benchmark_main)
  target_compile_options(benchmark::benchmark_main INTERFACE
    -Wno-weak-vtables
    -Wno-padded)
endif()

FetchContent_MakeAvailable(fetch_benchmark)
