include(deps/GTest)

add_executable(${CFD_TEST}
  solver/TDMA_test.cpp
)

target_include_directories(${CFD_TEST} SYSTEM PRIVATE
  $<TARGET_PROPERTY:GTest::gtest_main,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:mdspan,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(${CFD_TEST} PRIVATE
  GTest::gtest_main
  ${CFD_LIB}
  ${EXTRA_LIBS}
)

target_link_options(${CFD_TEST} PRIVATE
  $<$<CONFIG:DEBUG>:${LINK_OPTIONS}>
  $<$<CONFIG:RELEASE>:${LINK_OPTIONS}>
)

target_compile_options(${CFD_TEST} PRIVATE
  $<$<CONFIG:DEBUG>:${DEBUG_OPTIONS}>
  $<$<CONFIG:RELEASE>:${RELEASE_OPTIONS}>
  -Wno-global-constructors
)

include(GoogleTest)
gtest_discover_tests(${CFD_TEST})
