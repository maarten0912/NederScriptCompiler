#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void impl_matrix_multiply_sequential( float* A
                                    , float* B
                                    , float* C
                                    , unsigned int Am
                                    , unsigned int An
                                    , unsigned int Bn
                                    );
void impl_matrix_multiply_openmp( float* A
                                , float* B
                                , float* C
                                , unsigned int Am
                                , unsigned int An
                                , unsigned int Bn
                                );
void impl_matrix_multiply_opencl( float* A
                                , float* B
                                , float* C
                                , unsigned int Am
                                , unsigned int An
                                , unsigned int Bn
                                );

void impl_matrix_countzero_sequential( float* M
                                     , unsigned int Am
                                     , unsigned int An
                                     , unsigned int* Z
                                     );
void impl_matrix_countzero_openmp( float* M
                                 , unsigned int Am
                                 , unsigned int An
                                 , unsigned int* Z
                                 );
void impl_matrix_countzero_opencl( float* M
                                 , unsigned int Am
                                 , unsigned int An
                                 , unsigned int* Z
                                 );

void impl_vector_sort_sequential( float* V
                                , unsigned int Vn
                                );
void impl_vector_sort_openmp( float* V
                            , unsigned int Vn
                            );

void impl_vector_sum_sequential( float* V
                               , unsigned int Vn
                               , float* S
                               );
void impl_vector_sum_opencl( float* V
                           , unsigned int Vn
                           , float* S
                           );

#ifdef __cplusplus
}
#endif
