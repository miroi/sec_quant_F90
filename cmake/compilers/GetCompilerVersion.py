#!/usr/bin/env python
#
# Script to determine and verify compilers (C, C++ and Fortran) versions.
# For each vendor/system calls compiler specific command (like "gcc --version")
# returning desired compiler version.
#
# This script returns (as stdout) for CMake a list of 3 strings - compilers versions - separated by semicolons.
#
# Written by Ivan Hrasko, february 2014
#

import sys, subprocess, re

#! compiler vendor
C_COMPILER_VENDOR = sys.argv[1]
CXX_COMPILER_VENDOR = sys.argv[2]
Fortran_COMPILER_VENDOR = sys.argv[3]
#! operating system name
CMAKE_SYSTEM_NAME = sys.argv[4]

#! in every call both stdout and stderr are used and merged
#! this is really needed probably only by Intel which uses stderr, 
#! but it probably also makes no problem when used everywhere

#! looking for C compiler version
if C_COMPILER_VENDOR == 'GNU':
    try:
        outputGCC = subprocess.Popen(['gcc', '--version'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputGCC = ''
    
    matchGCC = re.search(r'\d+\.\d+\.\d+', outputGCC)
    if matchGCC:
        GCCversion = matchGCC.group()
    else:
        GCCversion = '0.0.0'

    C_COMPILER_VERSION = GCCversion
elif C_COMPILER_VENDOR == 'Intel':
    try:
        if CMAKE_SYSTEM_NAME == 'Windows':
            outputIntelC = subprocess.Popen(['icl', '/V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
        else:
            outputIntelC = subprocess.Popen(['icc', '-V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputIntelC = ''
    
    matchIntelC = re.search(r'\d+\.\d+', outputIntelC)
    if matchIntelC:
        IntelCversion = matchIntelC.group()
    else:
        IntelCversion = '0.0'
			
    C_COMPILER_VERSION = IntelCversion
elif C_COMPILER_VENDOR == 'PGI':
    try:
        outputPGIC = subprocess.Popen(['pgcc', '-V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputPGIC = ''
    
    matchPGIC = re.search(r'\d+\.\d+', outputPGIC)
    if matchPGIC:
        PGICversion = matchPGIC.group()
    else:
        PGICversion = '0.0'
			
    C_COMPILER_VERSION = PGICversion
elif C_COMPILER_VENDOR == 'XL':
    try:
        outputXLC = subprocess.Popen(['xlc', '-qversion'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputXLC = ''
    
    matchXLC = re.search(r'\d+\.\d+', outputXLC)
    if matchXLC:
        XLCversion = matchXLC.group()
    else:
        XLCversion = '0.0'
			
    C_COMPILER_VERSION = XLCversion
elif C_COMPILER_VENDOR == 'Clang':
    try:
        outputClangC = subprocess.Popen(['clang', '--version'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputClangC = ''
    
    matchClangC = re.search(r'\d+\.\d+', outputClangC)
    if matchClangC:
        ClangCversion = matchClangC.group()
    else:
        ClangCversion = '0.0'
			
    C_COMPILER_VERSION = ClangCversion
else:
    #! unknown, not supported vendor
    C_COMPILER_VERSION = '0.0'

#! looking for C++ compiler version
if CXX_COMPILER_VENDOR == 'GNU':
    try:
        outputGXX = subprocess.Popen(['g++', '--version'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputGXX = ''
    
    matchGXX = re.search(r'\d+\.\d+\.\d+', outputGXX)
    if matchGXX:
        GXXversion = matchGXX.group()
    else:
        GXXversion = '0.0.0'

    CXX_COMPILER_VERSION = GXXversion
elif CXX_COMPILER_VENDOR == 'Intel':
    try:
        if CMAKE_SYSTEM_NAME == 'Windows':
            outputIntelCXX = subprocess.Popen(['icl', '/V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
        else:
            outputIntelCXX = subprocess.Popen(['icpc', '-V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputIntelCXX = ''
    
    matchIntelCXX = re.search(r'\d+\.\d+', outputIntelCXX)
    if matchIntelCXX:
        IntelCXXversion = matchIntelCXX.group()
    else:
        IntelCXXversion = '0.0'
			
    CXX_COMPILER_VERSION = IntelCXXversion
elif CXX_COMPILER_VENDOR == 'PGI':
    try:
        outputPGICXX = subprocess.Popen(['pgCC', '-V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputPGICXX = ''
    
    matchPGICXX = re.search(r'\d+\.\d+', outputPGICXX)
    if matchPGICXX:
        PGICXXversion = matchPGICXX.group()
    else:
        PGICXXversion = '0.0'
			
    CXX_COMPILER_VERSION = PGICXXversion
elif CXX_COMPILER_VENDOR == 'XL':
    try:
        outputXLCXX = subprocess.Popen(['xlC', '-qversion'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputXLCXX = ''
    
    matchXLCXX = re.search(r'\d+\.\d+', outputXLCXX)
    if matchXLCXX:
        XLCXXversion = matchXLCXX.group()
    else:
        XLCXXversion = '0.0'
			
    CXX_COMPILER_VERSION = XLCXXversion
elif CXX_COMPILER_VENDOR == 'Clang':
    try:
        outputClangCXX = subprocess.Popen(['clang++', '--version'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputClangCXX = ''
    
    matchClangCXX = re.search(r'\d+\.\d+', outputClangCXX)
    if matchClangCXX:
        ClangCXXversion = matchClangCXX.group()
    else:
        ClangCXXversion = '0.0'
			
    CXX_COMPILER_VERSION = ClangCXXversion
else:
    #! unknown, not supported vendor
    CXX_COMPILER_VERSION = '0.0'

#! looking for Fortran compiler version
if Fortran_COMPILER_VENDOR == 'GNU':
    try:
        outputGFORTRAN = subprocess.Popen(['gfortran', '--version'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputGFORTRAN = ''
    
    matchGFORTRAN = re.search(r'\d+\.\d+\.\d+', outputGFORTRAN)
    if matchGFORTRAN:
        GFORTRANversion = matchGFORTRAN.group()
    else:
        GFORTRANversion = '0.0.0'

    Fortran_COMPILER_VERSION = GFORTRANversion
elif Fortran_COMPILER_VENDOR == 'Intel':
    try:
        if CMAKE_SYSTEM_NAME == 'Windows':
            outputIntelFORTRAN = subprocess.Popen(['ifort', '/V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
        else:
            outputIntelFORTRAN = subprocess.Popen(['ifort', '-V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputIntelFORTRAN = ''
    
    matchIntelFORTRAN = re.search(r'\d+\.\d+', outputIntelFORTRAN)
    if matchIntelFORTRAN:
        IntelFORTRANversion = matchIntelFORTRAN.group()
    else:
        IntelFORTRANversion = '0.0'
			
    Fortran_COMPILER_VERSION = IntelFORTRANversion
elif Fortran_COMPILER_VENDOR == 'PGI':
    try:
        outputPGIFORTRAN = subprocess.Popen(['pgf90', '-V'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputPGIFORTRAN = ''
    
    matchPGIFORTRAN = re.search(r'\d+\.\d+', outputPGIFORTRAN)
    if matchPGIFORTRAN:
        PGIFORTRANversion = matchPGIFORTRAN.group()
    else:
        PGIFORTRANversion = '0.0'
			
    Fortran_COMPILER_VERSION = PGIFORTRANversion
elif Fortran_COMPILER_VENDOR == 'XL':
    try:
        outputXLFORTRAN = subprocess.Popen(['xlf90', '-qversion'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()[0]
    except:
        outputXLFORTRAN = ''
    
    matchXLFORTRAN = re.search(r'\d+\.\d+', outputXLFORTRAN)
    if matchXLFORTRAN:
        XLFORTRANversion = matchXLFORTRAN.group()
    else:
        XLFORTRANversion = '0.0'
			
    Fortran_COMPILER_VERSION = XLFORTRANversion
else:
    #! unknown, not supported vendor
    Fortran_COMPILER_VERSION = '0.0'


#! this is the standard output for CMake (must be in this order and with ;)
print C_COMPILER_VERSION + ';' + CXX_COMPILER_VERSION + ';' + Fortran_COMPILER_VERSION
