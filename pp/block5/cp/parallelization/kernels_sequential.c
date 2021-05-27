#include <stdlib.h>
#include <stdio.h>
#include <kernels.h>

// A is Am * An matrix, B is array of length Bn
void impl_matrix_multiply_sequential( float* A
                                    , float* B
                                    , float* C
                                    , unsigned int Am
                                    , unsigned int An
                                    , unsigned int Bn
                                    ) {
    for(unsigned int i=0; i < Am; ++i) {
        for(unsigned int j=0; j < Bn; ++j) {
            C[i * Bn + j] = 0;
            for(unsigned int k = 0; k < An; ++k) {
                C[i * Bn + j] += A[i * An + k] * B[k * Bn + j];
            }
        }
    }
}

// M is Mm * Mn matrix
void impl_matrix_countzero_sequential( float* M
                                     , unsigned int Mm
                                     , unsigned int Mn
                                     , unsigned int* Z
                                     ) {
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
void impl_vector_sort_sequential( float* V
                                , unsigned int Vn
                                ) {

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

// V is array of length Vn
void impl_vector_sum_sequential( float* V
                               , unsigned int Vn
                               , float* S
                               ) {

    float sum = 0;
    float* e = V + Vn;

    while(e != V) {
        sum += *(V++);
    }

    *S = sum;
}
