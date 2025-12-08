include(deps/GTest)

add_executable(${CFD_TEST}
  solver/TDMA_test.cpp
)

target_link_libraries(${CFD_TEST} PRIVATE
  GTest::gtest_main
  ${CFD_LIB}
)

target_link_libraries(${CFD_TEST} PRIVATE ${EXTRA_LIBS})
target_link_libraries(${CFD_TEST} PUBLIC mdspan)

target_link_options(${CFD_TEST} PRIVATE $<$<CONFIG:DEBUG>:${LINK_OPTIONS}>)
target_link_options(${CFD_TEST} PRIVATE $<$<CONFIG:RELEASE>:${LINK_OPTIONS}>)

target_compile_options(${CFD_TEST} PRIVATE $<$<CONFIG:DEBUG>:${DEBUG_OPTIONS}>)
target_compile_options(${CFD_TEST} PRIVATE $<$<CONFIG:RELEASE>:${RELEASE_OPTIONS}>)

target_compile_options(${CFD_TEST} PRIVATE -Wno-global-constructors)

include(GoogleTest)
gtest_discover_tests(${CFD_TEST})
