set(FREE_FORTRAN_SOURCES
    code/control_print.F90
    code/evaluare_strings.F90
    code/indexed_quantities.F90
    code/kron_delta.F90
    code/operators_string.F90
    code/sec_quant_oper.F90
    code/dynamic_string.F90
    code/utils.F90
)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY
    ${PROJECT_BINARY_DIR}/lib
    )

set(CMAKE_Fortran_MODULE_DIRECTORY
    ${PROJECT_BINARY_DIR}/modules
    )

set(SECQUANT_SOURCES
    ${FREE_FORTRAN_SOURCES}
    )

add_library(
    secquant
    ${SECQUANT_SOURCES}
    ${GENERATED_FILES}
    )

set(LIST_OF_EXECUTABLES)
macro(compile_standalone _executable _source LIST_OF_EXECUTABLES)
    add_executable(
        ${_executable}
        ${_source}
    )
    set_property(TARGET ${_executable} PROPERTY LINKER_LANGUAGE Fortran)
    set(LIST_OF_EXECUTABLES
        ${LIST_OF_EXECUTABLES}
        ${_executable}
    )
endmacro()

compile_standalone(example_1.x examples/main_1.F90 "${LIST_OF_EXECUTABLES}")
compile_standalone(example_2.x examples/main_2.F90 "${LIST_OF_EXECUTABLES}")
compile_standalone(example_3.x examples/main_3.F90 "${LIST_OF_EXECUTABLES}")
compile_standalone(example_4.x examples/main_4.F90 "${LIST_OF_EXECUTABLES}")
compile_standalone(example_5.x examples/main_5.F90 "${LIST_OF_EXECUTABLES}")

foreach(
    EXECUTABLE
    ${LIST_OF_EXECUTABLES}
    )
    if(${CMAKE_SYSTEM_NAME} STREQUAL "AIX")
        SET_TARGET_PROPERTIES(${EXECUTABLE} PROPERTIES LINK_FLAGS
"-Wl,-bbigtoc")
    endif()
    if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
        SET_TARGET_PROPERTIES(${EXECUTABLE} PROPERTIES LINK_FLAGS "-Wl,-E")
    endif()
    target_link_libraries(
        ${EXECUTABLE}
        secquant
        ${EXTERNAL_LIBS}
        )

endforeach()
