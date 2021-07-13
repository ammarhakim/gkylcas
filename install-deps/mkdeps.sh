#!/bin/sh

# Defaults
PREFIX=$HOME/gkylsoft

# default build options
CC=gcc
CXX=g++

BUILD_CLN=no
BUILD_GINAC=no

# ----------------------------------------------------------------------------
# Function definitions
# ----------------------------------------------------------------------------

show_help() {
cat <<EOF

./mkdeps.sh CC=cc CXX=cxx

Build Gkyl-CAS dependencies.

CC 
CXX                         C and C++ compilers to use

-h
--help                      This help.
--prefix=DIR                Prefix where dependencies should be installed.
                            Default is $HOME/aaisoft

The following flags specify which libraries to build.

--build-cln                 [no] Should we build CLN?
--build-ginac               [no] Should we build Ginac?

EOF
}

# Helper functions

find_program() {
   prog=`command -v "$1" 2>/dev/null`
   if [ -n "$prog" ]
   then
      dirname "$prog"
   fi
}

die() {
   echo "$*"
   echo
   echo "Dependency builds failed."
   echo
   exit 1
}

# ----------------------------------------------------------------------------
# MAIN PROGRAM
# ----------------------------------------------------------------------------

# Parse options

while [ -n "$1" ]
do
   value="`echo $1 | sed 's/[^=]*.\(.*\)/\1/'`"
   key="`echo $1 | sed 's/=.*//'`"
   if `echo "$value" | grep "~" >/dev/null 2>/dev/null`
   then
      echo
      echo '*WARNING*: the "~" sign is not expanded in flags.'
      echo 'If you mean the home directory, use $HOME instead.'
      echo
   fi
   case "$key" in
   -h)
      show_help
      exit 0
      ;;
   --help)
      show_help
      exit 0
      ;;
   CC)
      [ -n "$value" ] || die "Missing value in flag $key."
      CC="$value"
      ;;
   CXX)
      [ -n "$value" ] || die "Missing value in flag $key."
      CXX="$value"
      ;;
   --prefix)
      [ -n "$value" ] || die "Missing value in flag $key."
      PREFIX="$value"
      ;;
   --build-cln)
      [ -n "$value" ] || die "Missing value in flag $key."
      BUILD_CLN="$value"
      ;;
   --build-ginac)
      [ -n "$value" ] || die "Missing value in flag $key."
      BUILD_GINAC="$value"
      ;;   
   *)
      die "Error: Unknown flag: $1"
      ;;
   esac
   shift
done

# Write out build options for scripts to use
cat <<EOF1 > build-opts.sh
# Generated automatically! Do not edit

# Installation directory
GKYLSOFT=$PREFIX
# Various compilers
CC=$CC
CXX=$CXX

EOF1

build_cln() {
    if [ "$BUILD_CLN" = "yes" ]
    then    
	echo "Building CLN"
	./build-cln.sh 
    fi
}

build_ginac() {
    if [ "$BUILD_GINAC" = "yes" ]
    then    
	echo "Building Ginac"
	./build-ginac.sh 
    fi
}

echo "Installations will be in  $PREFIX"

build_cln
build_ginac
