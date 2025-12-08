message(NOTICE "Fetching google/googletest from https://github.com/google/googletest ...")

FetchContent_Declare(
  fetch_googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        ${GTEST_TAG}
  GIT_SHALLOW ${FETCH_GIT_SHALLOW}
  GIT_PROGRESS ${FETCH_GIT_PROGRESS}
  UPDATE_DISCONNECTED ${FETCH_UPDATE_DISCONNECTED}
)

FetchContent_MakeAvailable(fetch_googletest)
