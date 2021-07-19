#
# You can set compiler to use as:
#
# make CC=mpicc 
#

CFLAGS = -O3 -g 
CXXFLAGS = -O3 -g 

GINAC_INC = ${HOME}/gkylsoft/ginac/include
CLN_INC = ${HOME}/gkylsoft/cln/include

GINAC_LIB_DIR = ${HOME}/gkylsoft/ginac/lib
CLN_LIB_DIR = ${HOME}/gkylsoft/cln/lib

KERN_INCLUDES = -Ikernels -Ikernels/basis -Ikernels/bin_op
INCLUDES = -Iunit -Ilib -I${GINAC_INC} -I${CLN_INC} 
LIBDIRS = -L${GINAC_LIB_DIR} -L${CLN_LIB_DIR}
PREFIX = ${HOME}/gkylsoft
LDFLAGS = "-Wl,-rpath,${CLN_LIB_DIR}"

%.o : %.c
	${CC} -c $(CFLAGS) $(INCLUDES) ${KERN_INCLUDES} -o $@ $<

%.o : %.cpp
	${CXX} -c $(CXXFLAGS) $(INCLUDES) -o $@ $<

# Header dependencies
headers = $(wildcard lib/*.h)

# Object files to compile in library
libobjs = $(patsubst %.cpp,%.o,$(wildcard lib/*.cpp))

# Make targets: libraries
all: build/libgkylcas.a \
	$(patsubst %.cxx,build/%,$(wildcard unit/cxxtest_*.cxx)) \
	$(patsubst %.cxx,build/%,$(wildcard codegen/codegen_*.cxx))

# Library archive
build/libgkylcas.a: ${libobjs} ${headers}
	ar -crs build/libgkylcas.a ${libobjs}

# Unit tests
build/unit/%: unit/%.cxx build/libgkylcas.a
	${CXX} ${CXXFLAGS} ${LIBDIRS} ${LDFLAGS} -o $@ $< -I. $(INCLUDES) -Lbuild -lgkylcas -lginac -lcln -lgmp

# Code generators
build/codegen/%: codegen/%.cxx build/libgkylcas.a
	${CXX} ${CXXFLAGS} ${LIBDIRS} ${LDFLAGS} -o $@ $< -I. $(INCLUDES) -Lbuild -lgkylcas -lginac -lcln -lgmp

compile-kernels: $(patsubst %.c,%.o,$(wildcard kernels/*/*.c))

.PHONY: clean clean-compile-kernels

clean:
	rm -rf build/libgkylcas.a */*.o build/unit/cxxtest_* build/codegen/codegen_*

clean-compile-kernels:
	rm -rf kernels/*/*.o
