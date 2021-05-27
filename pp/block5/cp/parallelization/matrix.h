#pragma once

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	unsigned int n;
	unsigned int m;
	float* data;
} Matrix;

typedef void (*f_matrix_multiply)( float* A
                                 , float* B
                                 , float* C
                                 , unsigned int Am
                                 , unsigned int An
                                 , unsigned int Bn
                                 );

typedef void (*f_matrix_countzero)( float* M
                                  , unsigned int Am
                                  , unsigned int An
                                  , unsigned int* Z
                                  );

/**
 * @brief Creates a new @c Matrix
 * @param m Number of rows of the new Matrix
 * @param n Number of columns of the new Matrix
 * @return New @c Matrix or NULL on failure
 */
Matrix* Matrix_new(int m, int n);
Matrix* Matrix_create(int m, int n, float* data);
void Matrix_delete(Matrix* M);
void Matrix_fill(Matrix* M, float value);
Matrix* Matrix_multiply(Matrix* A, Matrix* B, f_matrix_multiply f);
unsigned int Matrix_countzero(Matrix* M, f_matrix_countzero f);
void Matrix_print(Matrix* M, FILE* printFile);
void Matrix_print_listnonzero(Matrix* M, FILE* printFile);
int Matrix_compare(Matrix* A, Matrix* B);
Matrix* Matrix_diff(Matrix* A, Matrix* B);
int Matrix_print_diff(Matrix* A, Matrix* B, FILE* printFile);
void Matrix_save(Matrix* M, FILE* file);
Matrix* Matrix_load(FILE* file);

#ifdef __cplusplus
}
#endif
