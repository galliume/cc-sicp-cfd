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
    Mesh.cpp)

add_subdirectory(solver)

target_link_libraries(${CFD_LIB} PRIVATE ${EXTRA_LIBS})

target_compile_options(${CFD_LIB} PRIVATE
  $<$<CONFIG:DEBUG>:-g;-O0;-Werror>
  $<$<CONFIG:RELEASE>:-O3;-DNDEBUG>
  -Wno-c++98-compat
  -Weverything)
