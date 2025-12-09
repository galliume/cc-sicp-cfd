include(deps/GBM)


add_executable(${CFD_BENCHMARK}
TDMA_benchmark.cpp
)

target_include_directories(${CFD_BENCHMARK} SYSTEM PRIVATE
  $<TARGET_PROPERTY:benchmark::benchmark_main,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:mdspan,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(${CFD_BENCHMARK} PRIVATE
  benchmark::benchmark_main
  ${CFD_LIB}
  ${EXTRA_LIBS}
)
