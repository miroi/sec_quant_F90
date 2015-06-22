#!/usr/bin/env python

# This script is autogenerated by Autocmake
# Copyright (c) 2015 by Radovan Bast and Jonas Juselius
# See https://github.com/scisoft/autocmake/blob/master/LICENSE

import os
import sys

sys.path.append('cmake/lib')
from config import configure
from docopt import docopt


options = """
Usage:
  ./setup.py [options] [<builddir>]
  ./setup.py (-h | --help)

Options:
  --cc=<CC>                           C compiler [default: gcc].
  --extra-cc-flags=<EXTRA_CFLAGS>     Extra C compiler flags [default: ''].
  --coverage                          Enable code coverage [default: False].
  --cxx=<CXX>                         C++ compiler [default: g++].
  --extra-cxx-flags=<EXTRA_CXXFLAGS>  Extra C++ compiler flags [default: ''].
  --fc=<FC>                           Fortran compiler [default: gfortran].
  --extra-fc-flags=<EXTRA_FCFLAGS>    Extra Fortran compiler flags [default: ''].
  --int64                             Enable 64bit integers [default: False].
  --blas=<BLAS>                       Detect and link BLAS library (auto or off) [default: auto].
  --lapack=<LAPACK>                   Detect and link LAPACK library (auto or off) [default: auto].
  --mkl=<MKL>                         Pass MKL flag to the Intel compiler and linker and skip BLAS/LAPACK detection (sequential, parallel, cluster, or off) [default: off].
  --static                            Enable static linking [default: False].
  --show                              Show CMake command and exit.
  <builddir>                          Build directory.
  -h --help                           Show this screen.
"""


def gen_cmake_command(options, arguments):
    """
    Generate CMake command based on options and arguments.
    """
    command = []
    command.append('CC=%s' % arguments['--cc'])
    command.append('CXX=%s' % arguments['--cxx'])
    command.append('FC=%s' % arguments['--fc'])
    command.append('cmake')
    command.append('-DEXTRA_CFLAGS="%s"' % arguments['--extra-cc-flags'])
    command.append('-DENABLE_CODE_COVERAGE=%s' % arguments['--coverage'])
    command.append('-DEXTRA_CXXFLAGS="%s"' % arguments['--extra-cxx-flags'])
    command.append('-DEXTRA_FCFLAGS="%s"' % arguments['--extra-fc-flags'])
    command.append('-DENABLE_64BIT_INTEGERS=%s' % arguments['--int64'])
    command.append('-DENABLE_BLAS=%s' % arguments['--blas'])
    command.append('-DENABLE_LAPACK=%s' % arguments['--lapack'])
    command.append('-DMKL_FLAG=%s' % arguments['--mkl'])
    command.append('-DMATH_LIB_SEARCH_ORDER="MKL;ESSL;ATLAS;ACML;SYSTEM_NATIVE"')
    command.append('-DBLAS_LANG=Fortran')
    command.append('-DLAPACK_LANG=Fortran')
    command.append('-DENABLE_STATIC_LINKING=%s' % arguments['--static'])

    return ' '.join(command)


arguments = docopt(options, argv=None)

root_directory = os.path.dirname(os.path.realpath(__file__))
build_path = arguments['<builddir>']
cmake_command = '%s %s' % (gen_cmake_command(options, arguments), root_directory)
configure(root_directory, build_path, cmake_command, arguments['--show'])
