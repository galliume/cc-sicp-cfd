set(FETCHCONTENT_QUIET OFF)

set(FETCH_GIT_SHALLOW TRUE)
set(FETCH_GIT_PROGRESS TRUE)
set(FETCH_UPDATE_DISCONNECTED TRUE)

set(KOKKOSMDSPAN_TAG stable)
set(GTEST_TAG v1.17.0)
set(GBM_TAG v1.9.4)

include(FetchContent)

include(deps/KokkosMDSpan)
#include(deps/GTest)