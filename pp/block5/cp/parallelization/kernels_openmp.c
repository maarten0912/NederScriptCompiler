#include <kernels.h>
#include <stdio.h>
#include <omp.h>

// A is Am * An matrix, B is array of length Bn
void impl_matrix_multiply_openmp( float* A
                                , float* B
                                , float* C
                                , unsigned int Am
                                , unsigned int An
                                , unsigned int Bn
                                ) {
    // Modify this implementation for 5-CP.1                                                        
    unsigned int i, j, k;

    for(i=0; i < Am; ++i) {
        for(j=0; j < Bn; ++j) {
            C[i * Bn + j] = 0;
            for(k = 0; k < An; ++k) {
                C[i * Bn + j] += A[i * An + k] * B[k * Bn + j];
            }
        }
    }

}

// M is Mm * Mn matrix
// Result should be stored in Z. 
void impl_matrix_countzero_openmp( float* M
                                 , unsigned int Mm
                                 , unsigned int Mn
                                 , unsigned int* Z
                                 ) {
    // Modify this implementation for 5-CP.2                                                          
    unsigned int z = 0;
    for(unsigned int i=0; i < Mm*Mn; ++i) {
        if(M[i] == 0.0f) z++;
    }
    *Z = z;

}

#define swap(a,b) do { \
  float tmp = a; \
  a = b; \
  b = tmp; \
} while(0)

// V is array of length Vn
void impl_vector_sort_openmp( float* V
                            , unsigned int Vn
                            ) {
    // Modify this implementation for 5-CP.3
    int sorted = 0;
    while(!sorted) {
        sorted = 1;
        for(unsigned int i = 1; i < Vn-1; i += 2) {
            if(V[i] > V[i+1]) {
                swap(V[i], V[i+1]);
                sorted = 0;
            }
        }
        for(unsigned int i = 0; i < Vn-1; i += 2) {
            if(V[i] > V[i+1]) {
                swap(V[i], V[i+1]);
                sorted = 0;
            }
        }
    }

}

