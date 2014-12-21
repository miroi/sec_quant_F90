message("-- System                : ${CMAKE_SYSTEM_NAME}")
message("-- Processor type        : ${CMAKE_HOST_SYSTEM_PROCESSOR}")
message("-- Fortran compiler flags: ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_${cmake_build_type_toupper}}")
message("-- C compiler flags      : ${CMAKE_C_FLAGS}       ${CMAKE_C_FLAGS_${cmake_build_type_toupper}}")
message("-- C++ compiler flags    : ${CMAKE_CXX_FLAGS}     ${CMAKE_CXX_FLAGS_${cmake_build_type_toupper}}")
message("-- Libraries             : ${EXTERNAL_LIBS}")

get_directory_property(_list_of_definitions DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS)
message("-- Definitions           : ${_list_of_definitions}")
unset(_list_of_definitions)

# get size of static allocations
foreach(_binary ${STATIC_MEM_INFO_BINARIES})
    add_custom_target(
        static_mem_${_binary}
            COMMAND ${CMAKE_SOURCE_DIR}/cmake/binary-info/get_static_size.py ${_binary}.x
        )
endforeach()
