message(NOTICE "Fetching kokkos/mdspan from https://github.com/kokkos/mdspan ...")

FetchContent_Declare(
  fetch_mdspan
  GIT_REPOSITORY https://github.com/kokkos/mdspan.git
  GIT_TAG        ${KOKKOSMDSPAN_TAG}
  GIT_SHALLOW ${FETCH_GIT_SHALLOW}
  GIT_PROGRESS ${FETCH_GIT_PROGRESS}
  UPDATE_DISCONNECTED ${FETCH_UPDATE_DISCONNECTED}
)
FetchContent_MakeAvailable(fetch_mdspan)

if(TARGET mdspan)
  target_compile_options(mdspan INTERFACE
    -Wno-missing-noreturn
    -Wno-documentation
    -Wno-documentation-unknown-command
    -Wno-pre-c++14-compat
    -Wno-c++98-compat
    -Wno-c++98-compat-pedantic
    -Wno-decls-in-multiple-modules
    -Wno-unsafe-buffer-usage-in-libc-call
    -Wno-old-style-cast
    -Wno-unsafe-buffer-usage)
endif()

target_link_libraries(${PROJECT_NAME}
PRIVATE
  mdspan)