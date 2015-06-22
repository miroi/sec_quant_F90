#.rst:
#
# Detects and links to BLAS and LAPACK libraries.
#
# Variables used::
#
#   MATH_LIB_SEARCH_ORDER, example: set(MATH_LIB_SEARCH_ORDER MKL ESSL ATLAS ACML SYSTEM_NATIVE)
#   ENABLE_BLAS
#   ENABLE_LAPACK
#   BLAS_FOUND
#   LAPACK_FOUND
#   BLAS_LANG
#   LAPACK_LANG
#   BLAS_TYPE
#   LAPACK_TYPE
#   ENABLE_64BIT_INTEGERS
#   CMAKE_HOST_SYSTEM_PROCESSOR
#   BLACS_IMPLEMENTATION
#   MKL_FLAG
#
# Variables defined::
#
#   MATH_LIBS
#   BLAS_FOUND
#   LAPACK_FOUND
#
# Environment variables used::
#
#   MATH_ROOT
#   BLAS_ROOT
#   LAPACK_ROOT
#   MKL_ROOT
#   MKLROOT
#
# Example autocmake.cfg entry::
#
#   [math_libs]
#   source: https://github.com/scisoft/autocmake/raw/master/modules/math_libs.cmake
#   docopt: --blas=<BLAS> Detect and link BLAS library (auto or off) [default: auto].
#           --lapack=<LAPACK> Detect and link LAPACK library (auto or off) [default: auto].
#           --mkl=<MKL> Pass MKL flag to the Intel compiler and linker and skip BLAS/LAPACK detection (sequential, parallel, cluster, or off) [default: off].
#   define: '-DENABLE_BLAS=%s' % arguments['--blas']
#           '-DENABLE_LAPACK=%s' % arguments['--lapack']
#           '-DMKL_FLAG=%s' % arguments['--mkl']
#           '-DMATH_LIB_SEARCH_ORDER="MKL;ESSL;ATLAS;ACML;SYSTEM_NATIVE"'
#           '-DBLAS_LANG=Fortran'
#           '-DLAPACK_LANG=Fortran'

#-------------------------------------------------------------------------------
# SYSTEM_NATIVE

set(SYSTEM_NATIVE_BLAS_INCLUDE_PATH_SUFFIXES)
set(SYSTEM_NATIVE_LAPACK_INCLUDE_PATH_SUFFIXES)

set(SYSTEM_NATIVE_BLAS_HEADERS   cblas.h)
set(SYSTEM_NATIVE_LAPACK_HEADERS clapack.h)

set(SYSTEM_NATIVE_BLAS_LIBRARY_PATH_SUFFIXES)
set(SYSTEM_NATIVE_LAPACK_LIBRARY_PATH_SUFFIXES)

set(SYSTEM_NATIVE_BLAS_LIBS   blas)
set(SYSTEM_NATIVE_LAPACK_LIBS lapack)

#-------------------------------------------------------------------------------
# ESSL

set(ESSL_BLAS_INCLUDE_PATH_SUFFIXES)
set(ESSL_LAPACK_INCLUDE_PATH_SUFFIXES)

set(ESSL_BLAS_HEADERS   UNKNOWN)
set(ESSL_LAPACK_HEADERS UNKNOWN)

set(ESSL_BLAS_LIBRARY_PATH_SUFFIXES)
set(ESSL_LAPACK_LIBRARY_PATH_SUFFIXES)

if(ENABLE_64BIT_INTEGERS)
   set(ESSL_BLAS_LIBS   esslsmp6464)
   set(ESSL_LAPACK_LIBS esslsmp6464)
else()
   set(ESSL_BLAS_LIBS   essl)
   set(ESSL_LAPACK_LIBS essl)
endif()

#-------------------------------------------------------------------------------
# ACML

set(ACML_BLAS_INCLUDE_PATH_SUFFIXES)
set(ACML_LAPACK_INCLUDE_PATH_SUFFIXES)

set(ACML_BLAS_HEADERS   cblas.h)
set(ACML_LAPACK_HEADERS clapack.h)

set(ACML_BLAS_LIBRARY_PATH_SUFFIXES   libso)
set(ACML_LAPACK_LIBRARY_PATH_SUFFIXES libso)

set(ACML_BLAS_LIBS   acml)
set(ACML_LAPACK_LIBS acml)

#-------------------------------------------------------------------------------
# ATLAS

set(ATLAS_BLAS_INCLUDE_PATH_SUFFIXES   atlas)
set(ATLAS_LAPACK_INCLUDE_PATH_SUFFIXES atlas)

set(ATLAS_BLAS_HEADERS   cblas.h)
set(ATLAS_LAPACK_HEADERS clapack.h)

set(ATLAS_BLAS_LIBRARY_PATH_SUFFIXES   atlas atlas-base atlas-base/atlas atlas-sse3)
set(ATLAS_LAPACK_LIBRARY_PATH_SUFFIXES atlas atlas-base atlas-base/atlas atlas-sse3)

set(ATLAS_BLAS_LIBS   f77blas cblas atlas)
set(ATLAS_LAPACK_LIBS atlas lapack)

#-------------------------------------------------------------------------------
# OPENBLAS (no LAPACK in OPENBLAS)

set(OPENBLAS_BLAS_INCLUDE_PATH_SUFFIXES)

set(OPENBLAS_BLAS_HEADERS cblas_openblas.h)

set(OPENBLAS_BLAS_LIBRARY_PATH_SUFFIXES)

set(OPENBLAS_BLAS_LIBS openblas)

#-------------------------------------------------------------------------------
# MKL

set(MKL_BLAS_INCLUDE_PATH_SUFFIXES)
set(MKL_LAPACK_INCLUDE_PATH_SUFFIXES)

set(MKL_BLAS_HEADERS   mkl_cblas.h)
set(MKL_LAPACK_HEADERS mkl_lapack.h)

if(${CMAKE_HOST_SYSTEM_PROCESSOR} STREQUAL "x86_64")
    set(MKL_BLAS_LIBRARY_PATH_SUFFIXES   intel64 em64t)
    set(MKL_LAPACK_LIBRARY_PATH_SUFFIXES intel64 em64t)
else()
    set(MKL_BLAS_LIBRARY_PATH_SUFFIXES   ia32 32)
    set(MKL_LAPACK_LIBRARY_PATH_SUFFIXES ia32 32)
endif()

set(MKL_COMPILER_BINDINGS ${CMAKE_${BLAS_LANG}_COMPILER_ID})

set(_thread_lib)
if(MKL_COMPILER_BINDINGS MATCHES Intel)
    set(_thread_lib mkl_intel_thread)
endif()
if(MKL_COMPILER_BINDINGS MATCHES PGI)
    set(_thread_lib mkl_pgi_thread)
endif()
if(MKL_COMPILER_BINDINGS MATCHES GNU)
    set(_thread_lib mkl_gnu_thread)
endif()
if(MKL_COMPILER_BINDINGS MATCHES Clang)
    set(_thread_lib mkl_gnu_thread)
endif()

if(MKL_COMPILER_BINDINGS MATCHES Intel)
    set(_compiler_mkl_interface mkl_intel)
endif()
if(MKL_COMPILER_BINDINGS MATCHES PGI)
    set(_compiler_mkl_interface mkl_intel)
endif()
if(MKL_COMPILER_BINDINGS MATCHES GNU)
    set(_compiler_mkl_interface mkl_gf)
endif()
if(MKL_COMPILER_BINDINGS MATCHES Clang)
    set(_compiler_mkl_interface mkl_gf)
endif()

set(_lib_suffix)
if(${CMAKE_HOST_SYSTEM_PROCESSOR} STREQUAL "x86_64")
    if(ENABLE_64BIT_INTEGERS)
        set(_lib_suffix _ilp64)
    else()
        set(_lib_suffix _lp64)
    endif()
endif()

if(ENABLE_SCALAPACK)
    set(_scalapack_lib      mkl_scalapack${_lib_suffix})
    if(${BLACS_IMPLEMENTATION} STREQUAL "intelmpi")
        set(_blacs_lib mkl_blacs_intelmpi${_lib_suffix})
    elseif(${BLACS_IMPLEMENTATION} STREQUAL "openmpi")
        set(_blacs_lib mkl_blacs_openmpi${_lib_suffix})
    elseif(${BLACS_IMPLEMENTATION} STREQUAL "sgimpt")
        set(_blacs_lib mkl_blacs_sgimpt${_lib_suffix})
    else()
        message(FATAL_ERROR "BLACS implementation ${BLACS_IMPLEMENTATION} not recognized/supported")
    endif()
else()
    set(_scalapack_lib)
    set(_blacs_lib)
endif()

# MKL 10.0.1.014
set(MKL_BLAS_LIBS ${_scalapack_lib} ${_compiler_mkl_interface}${_lib_suffix} ${_thread_lib} mkl_core mkl_def mkl_mc ${_blacs_lib} guide pthread m)
# then try this MKL BLAS combination
set(MKL_BLAS_LIBS2 ${_scalapack_lib} ${_compiler_mkl_interface}${_lib_suffix} ${_thread_lib} mkl_core ${_blacs_lib}   guide pthread m)
# newer MKL BLAS versions do not have libguide
set(MKL_BLAS_LIBS3 ${_scalapack_lib} ${_compiler_mkl_interface}${_lib_suffix} ${_thread_lib} mkl_core ${_blacs_lib}         pthread m)
# ancient MKL BLAS
set(MKL_BLAS_LIBS4 mkl guide m)

set(MKL_LAPACK_LIBS mkl_lapack95${_lib_suffix} ${_compiler_mkl_interface}${_lib_suffix})
# older MKL LAPACK
set(MKL_LAPACK_LIBS2 mkl_lapack)

unset(_lib_suffix)
unset(_thread_lib)
unset(_compiler_mkl_interface)
unset(_scalapack_lib)
unset(_blacs_lib)

macro(find_math_header _service _header)
    string(TOUPPER ${_service} _SERVICE)
    find_path(${_SERVICE}_INCLUDE_DIRS
        NAMES ${_header}
        PATHS ${${_SERVICE}_ROOT}
        HINTS ${${_SERVICE}_ROOT}/include
        PATH_SUFFIXES ${MATH_INCLUDE_PATH_SUFFIXES}
        NO_DEFAULT_PATH
        )
    # the following is needed for Atlas' clapack.h
    # this whole code needs major cleanup soon (2014-10-31)
    find_path(${_SERVICE}_INCLUDE_DIRS
        NAMES ${_header}
        PATH_SUFFIXES ${MATH_INCLUDE_PATH_SUFFIXES}
        )
    find_path(${_SERVICE}_INCLUDE_DIRS
        NAMES ${_header}
        PATH_SUFFIXES include
        )
    set(${_SERVICE}_H ${_header})
    unset(_SERVICE)
endmacro()

macro(find_math_libs _service)
    string(TOUPPER ${_service} _SERVICE)
    if(${_SERVICE}_FOUND)
        return()
    endif()
    set(_lib)
    set(_libs)
    foreach(l ${ARGN})
        find_library(_lib
            NAMES ${l}
            PATHS ${${_SERVICE}_ROOT}
            HINTS ${${_SERVICE}_ROOT}/lib64 ${${_SERVICE}_ROOT}/lib
            PATH_SUFFIXES ${MATH_LIBRARY_PATH_SUFFIXES}
            NO_DEFAULT_PATH
            )
        find_library(_lib
            NAMES ${l}
            PATH_SUFFIXES ${MATH_LIBRARY_PATH_SUFFIXES}
            )
        if(_lib)
            set(_libs ${_libs} ${_lib})
        else()
            set(${_SERVICE}_LIBRARIES ${_SERVICE}_LIBRARIES-NOTFOUND)
            set(_libs ${_SERVICE}_LIBRARIES-NOTFOUND)
            break()
        endif()
        unset(_lib CACHE)
    endforeach()
    set(${_SERVICE}_LIBRARIES ${_libs})
    unset(_lib CACHE)
    unset(_libs CACHE)
    unset(_SERVICE)
    unset(l)
endmacro()

macro(cache_math_result _service MATH_TYPE)
    string(TOUPPER ${_service} _SERVICE)
    set(${_SERVICE}_FIND_QUIETLY TRUE)
    if(DEFINED ${_SERVICE}_INCLUDE_DIRS)
        find_package_handle_standard_args(
            ${_SERVICE}
            "Could NOT find ${MATH_TYPE} ${_SERVICE}"
            ${_SERVICE}_LIBRARIES
            ${_SERVICE}_INCLUDE_DIRS
            )
    else()
        find_package_handle_standard_args(
            ${_SERVICE}
            "Could NOT find ${MATH_TYPE} ${_SERVICE}"
            ${_SERVICE}_LIBRARIES
            )
    endif()

    if(${_SERVICE}_FOUND)
        set(${_SERVICE}_TYPE ${MATH_TYPE} CACHE STRING
            "${_SERVICE} type")
        mark_as_advanced(${_SERVICE}_TYPE)

        add_definitions(-DHAVE_${MATH_TYPE}_${_SERVICE})
        set(HAVE_${_SERVICE} ON CACHE INTERNAL
            "Defined if ${_SERVICE} is available"
            )
        set(HAVE_${MATH_TYPE}_${_SERVICE} ON CACHE INTERNAL
            "Defined if ${MATH_TYPE}_${_SERVICE} is available"
            )
        set(${_SERVICE}_LIBRARIES ${${_SERVICE}_LIBRARIES} CACHE STRING
            "${_SERVICE} libraries"
            )
        mark_as_advanced(${_SERVICE}_LIBRARIES)
        if(DEFINED ${_SERVICE}_INCLUDE_DIRS)
            set(${_SERVICE}_H ${${_SERVICE}_H} CACHE STRING
                "${_SERVICE} header file")
            mark_as_advanced(${_SERVICE}_H)
            set(${_SERVICE}_INCLUDE_DIRS ${${_SERVICE}_INCLUDE_DIRS}
                CACHE STRING "${_SERVICE} include directory"
                )
            mark_as_advanced(${_SERVICE}_INCLUDE_DIRS)
        endif()
    else()
        set(${_SERVICE}_LIBRARIES ${_SERVICE}_LIBRARIES-NOTFOUND)
        if(DEFINED ${_SERVICE}_H)
            set(${_SERVICE}_INCLUDE_DIRS ${_SERVICE}_INCLUDE_DIRS-NOTFOUND)
            unset(${_SERVICE}_H)
        endif()
    endif()
    set(${_SERVICE}_FOUND ${${_SERVICE}_FOUND} PARENT_SCOPE)
    unset(MATH_TYPE)
    unset(_SERVICE)
endmacro()

macro(config_math_service _SERVICE)
    if(EXISTS $ENV{MATH_ROOT})
        if("${${_SERVICE}_ROOT}" STREQUAL "")
            set(${_SERVICE}_ROOT $ENV{MATH_ROOT})
            message("-- ${_SERVICE} will be searched for based on MATH_ROOT=${${_SERVICE}_ROOT} ")
        endif()
    endif()

    if(EXISTS $ENV{${_SERVICE}_ROOT})
        if("${${_SERVICE}_ROOT}" STREQUAL "")
            set(${_SERVICE}_ROOT $ENV{${_SERVICE}_ROOT})
            message("-- ${_SERVICE} will be searched for based on ${_SERVICE}_ROOT=${${_SERVICE}_ROOT}")
        endif()
    endif()

    if(EXISTS $ENV{MKL_ROOT})
        if("${${_SERVICE}_ROOT}" STREQUAL "")
            set(${_SERVICE}_ROOT $ENV{MKL_ROOT})
            message("-- ${_SERVICE} will be searched for based on MKL_ROOT=${${_SERVICE}_ROOT}")
        endif()
    endif()

    if(EXISTS $ENV{MKLROOT})
        if("${${_SERVICE}_ROOT}" STREQUAL "")
            set(${_SERVICE}_ROOT $ENV{MKLROOT})
            message("-- ${_SERVICE} will be searched for based on MKLROOT=${${_SERVICE}_ROOT}")
        endif()
    endif()

    if(${_SERVICE}_INCLUDE_DIRS AND ${_SERVICE}_LIBRARIES)
        set(${_SERVICE}_FIND_QUIETLY TRUE)
    endif()

    if(NOT ${_SERVICE}_FIND_COMPONENTS)
        if(DEFINED ${_SERVICE}_TYPE)
            set(${_SERVICE}_FIND_COMPONENTS ${${_SERVICE}_TYPE})
        else()
            set(${_SERVICE}_FIND_COMPONENTS ${MATH_LIB_SEARCH_ORDER})
        endif()
    endif()

    find_service(${_SERVICE})

    if(${_SERVICE}_FOUND)

        # take care of omp flags
        set(_omp_flag)
        if(HAVE_MKL_BLAS OR HAVE_MKL_LAPACK)
            if(MKL_COMPILER_BINDINGS MATCHES Intel)
                set(_omp_flag -openmp)
            endif()
            if(MKL_COMPILER_BINDINGS MATCHES GNU)
                set(_omp_flag -fopenmp)
            endif()
            if(MKL_COMPILER_BINDINGS MATCHES PGI)
                set(_omp_flag -mp)
            endif()
        endif()
        if(HAVE_MKL_${_SERVICE})
            set(${_SERVICE}_LIBRARIES -Wl,--start-group ${${_SERVICE}_LIBRARIES} ${_omp_flag} -Wl,--end-group)
        endif()
        unset(_omp_flag)

        find_package_message(${_SERVICE}
            "Found ${_SERVICE}: ${${_SERVICE}_TYPE} (${${_SERVICE}_LIBRARIES})"
            "[${${_SERVICE}_LIBRARIES}]"
            )

        set(MATH_LIBS
            ${MATH_LIBS}
            ${${_SERVICE}_LIBRARIES}
            )
    else()
        add_definitions(-DUSE_BUILTIN_${_SERVICE})
        set(USE_BUILTIN_${_SERVICE} TRUE)
    endif()
endmacro()

macro(find_math_library _myservice _mytype)
    set(MATH_INCLUDE_PATH_SUFFIXES ${${_mytype}_${_myservice}_INCLUDE_PATH_SUFFIXES})
    if(${_myservice}_LANG STREQUAL "C")
        find_math_header(${_myservice} ${${_mytype}_${_myservice}_HEADERS})
    endif()
    set(MATH_LIBRARY_PATH_SUFFIXES ${${_mytype}_${_myservice}_LIBRARY_PATH_SUFFIXES})

    find_math_libs(${_myservice} ${${_mytype}_${_myservice}_LIBS})
    # try some alternative patterns (if defined) until we find it
    foreach(_i 2 3 4 5 6 7 8 9)
        if(NOT ${_myservice}_LIBRARIES)
            if(DEFINED ${_mytype}_${_myservice}_LIBS${_i})
                find_math_libs(${_myservice} ${${_mytype}_${_myservice}_LIBS${_i}})
            endif()
        endif()
    endforeach()
endmacro()

function(find_service _myservice)
    foreach(_component ${${_myservice}_FIND_COMPONENTS})
        find_math_library(${_myservice} ${_component})
        cache_math_result(${_myservice} ${_component})
        if(${_myservice}_FOUND)
            break()
        endif()
    endforeach()
endfunction()

foreach(_service BLAS LAPACK)
    if(NOT ${_service}_LANG)
        set(${_service}_LANG C)
    elseif(${_service}_LANG STREQUAL "C" OR ${_service}_LANG STREQUAL "CXX")
        set(${_service}_LANG C)
    elseif(NOT ${_service}_LANG STREQUAL "Fortran")
        message(FATAL_ERROR "Invalid ${_service} library linker language: ${${_service}_LANG}")
    endif()
endforeach()

set(MATH_LIBS)

if(NOT MKL_FLAG STREQUAL "off")
    set(EXTERNAL_LIBS ${EXTERNAL_LIBS} -mkl=${MKL_FLAG})
    message(STATUS "User set explicit MKL flag which is passed to the compiler and linker: -mkl=${MKL_FLAG}")
    message(STATUS "This disables math detection and builtin math libraries")
    message(STATUS "Setting -DHAVE_MKL_BLAS and -DHAVE_MKL_LAPACK")
    add_definitions(-DHAVE_MKL_BLAS)
    add_definitions(-DHAVE_MKL_LAPACK)
    set(BLAS_FOUND TRUE)
    set(LAPACK_FOUND TRUE)
endif()

if(ENABLE_BLAS STREQUAL "auto" OR ENABLE_LAPACK STREQUAL "auto" AND NOT ${_service}_FOUND)
    message(STATUS "Math lib search order is ${MATH_LIB_SEARCH_ORDER}")
endif()

foreach(_service BLAS LAPACK)
    if(ENABLE_${_service} STREQUAL "auto" AND NOT ${_service}_FOUND)
        config_math_service(${_service})
        if(${_service}_FOUND)
            include_directories(${${_service}_INCLUDE_DIRS})
        endif()
    endif()
endforeach()