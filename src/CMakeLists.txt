set(EXTERNAL_LIBS)

set(FREE_FORTRAN_SOURCES
    control_print.F90
    evaluare_strings.F90
    indexed_quantities.F90
    kron_delta.F90
    operators_string.F90
    sec_quant_oper.F90
    dynamic_string.F90
    utils.F90
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

compile_standalone(secquant.x main.F90 "${LIST_OF_EXECUTABLES}")

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

    if(ENABLE_STATIC_LINKING)
        target_link_libraries(${EXECUTABLE} ${NODYNEXPORT})
    endif()

endforeach()
