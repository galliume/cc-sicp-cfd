target_sources(${CFD_LIB}
  PUBLIC
    FILE_SET cxx_modules TYPE CXX_MODULES
      FILES
        Schemes.cppm
        TDMA.cppm
  PRIVATE
    )
