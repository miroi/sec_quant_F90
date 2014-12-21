find_package(Sphinx QUIET)
if(SPHINX_FOUND)
    add_custom_target(
        html
        COMMAND python ${CMAKE_SOURCE_DIR}/doc/preprocess_sphinx ${CMAKE_SOURCE_DIR}/doc ${PROJECT_BINARY_DIR}
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        )
else()
    add_custom_target(
        html
        COMMAND echo error: please install python-sphinx and python-matplotlib first
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        )
endif()

find_package(Doxygen QUIET)
if(DOXYGEN_FOUND)
    configure_file(
        ${CMAKE_SOURCE_DIR}/doc/Doxyfile.in
        ${PROJECT_BINARY_DIR}/Doxyfile
    )
    add_custom_target(
        doxygen
        COMMAND ${DOXYGEN_EXECUTABLE} -d validate -d filteroutput -d commentscan
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        )
endif()
