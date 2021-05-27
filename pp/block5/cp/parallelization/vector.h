#pragma once

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*f_vector_sort)( float* V
                             , unsigned int Vn
                             );

typedef void (*f_vector_sum)( float* V
                            , unsigned int Vn
                            , float* S
                            );

typedef struct {
    unsigned int n;
    float* data;
} Vector;

Vector* Vector_new(int n);
void Vector_delete(Vector* V);
Vector* Vector_copy(Vector* V);
void Vector_sort(Vector* V, f_vector_sort kernel);
int Vector_compare(Vector* A, Vector* B);
void Vector_print(Vector* V, FILE* printFile);
float Vector_sum(Vector* V, f_vector_sum f);

#ifdef __cplusplus
}
#endif
