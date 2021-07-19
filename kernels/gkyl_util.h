// This stripped-down version of gkyl_util.h file allows building
// generated kernels for testing

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <time.h>

#ifdef __cplusplus

// extern "C" guards needed when using code from C++
# define EXTERN_C_BEG extern "C" {
# define EXTERN_C_END }

#else

# define EXTERN_C_BEG 
# define EXTERN_C_END

#endif

// restrict keyword in C and C++ are different
#ifdef __cplusplus
# define GKYL_RESTRICT __restrict__
#else
# define GKYL_RESTRICT restrict
#endif

// Maximum configuration-space dimensions supported
#ifndef GKYL_MAX_CDIM
# define GKYL_MAX_CDIM 3
#endif

// Maximum dimensions supported
#ifndef GKYL_MAX_DIM
# define GKYL_MAX_DIM 7
#endif

// Maximum number of supported species
#ifndef GKYL_MAX_SPECIES
# define GKYL_MAX_SPECIES 8
#endif

// Default alignment boundary
#ifndef GKYL_DEF_ALIGN
# define GKYL_DEF_ALIGN 64
#endif

// CUDA specific defines etc
#ifdef __NVCC__

#include <cuda_runtime.h>

#define GKYL_HAVE_CUDA

#define GKYL_CU_DH __device__ __host__
#define GKYL_CU_D __device__ 

// for directional copies
enum gkyl_cu_memcpy_kind {
  GKYL_CU_MEMCPY_H2H = cudaMemcpyHostToHost,
  GKYL_CU_MEMCPY_H2D = cudaMemcpyHostToDevice,
  GKYL_CU_MEMCPY_D2H = cudaMemcpyDeviceToHost,
  GKYL_CU_MEMCPY_D2D = cudaMemcpyDeviceToDevice
};

#define GKYL_DEFAULT_NUM_THREADS 256

#else

#undef GKYL_HAVE_CUDA
#define GKYL_CU_DH
#define GKYL_CU_D

// for directional copies
enum gkyl_cu_memcpy_kind {
  GKYL_CU_MEMCPY_H2H,
  GKYL_CU_MEMCPY_H2D,
  GKYL_CU_MEMCPY_D2H,
  GKYL_CU_MEMCPY_D2D,
};

#define GKYL_DEFAULT_NUM_THREADS 1

#endif // CUDA specific defines etc

