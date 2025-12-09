add_library(${CFD_LIB} STATIC)

target_sources(${CFD_LIB}
  PUBLIC
    FILE_SET cxx_modules TYPE CXX_MODULES
      BASE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}
      FILES
        Defines.cppm
        Field.cppm
        Mesh.cppm
        Physics.cppm
  PRIVATE
    )

add_subdirectory(solver)

target_link_libraries(${CFD_LIB} PRIVATE ${EXTRA_LIBS})
target_link_libraries(${CFD_LIB} PUBLIC mdspan)

target_link_options(${CFD_LIB} PRIVATE $<$<CONFIG:DEBUG>:${LINK_OPTIONS}>)
target_link_options(${CFD_LIB} PRIVATE $<$<CONFIG:RELEASE>:${LINK_OPTIONS}>)

target_compile_options(${CFD_LIB} PRIVATE $<$<CONFIG:DEBUG>:${DEBUG_OPTIONS}>)
target_compile_options(${CFD_LIB} PRIVATE $<$<CONFIG:RELEASE>:${RELEASE_OPTIONS}>)
