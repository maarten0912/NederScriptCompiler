#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include <matrix.h>

#define MATRIX_ELEMENT_T float

Matrix* Matrix_new(int m, int n) {
    Matrix* M = (Matrix*)malloc(sizeof(Matrix));
    M->m = m;
    M->n = n;
    M->data = (MATRIX_ELEMENT_T*)malloc(m*n*sizeof(MATRIX_ELEMENT_T));
    return M;
}

Matrix* Matrix_create(int m, int n, float* data) {
    Matrix* M = Matrix_new(m, n);
    memcpy(M->data, data, m*n*sizeof(MATRIX_ELEMENT_T));
    return M;
}

void Matrix_delete(Matrix* M) {
    if(M) {
        if(M->data) free(M->data);
        free(M);
    }
}

void Matrix_fill(Matrix* M, MATRIX_ELEMENT_T value) {
    if(value==0) {
        memset(M->data, value, M->m * M->n * sizeof(MATRIX_ELEMENT_T));
    } else {
        for(unsigned int i=0; i < M->m; ++i) {
            for(unsigned int j=0; i < M->n; ++j) {
                M->data[i + M->m * j] = value;
            }
        }
    }
}

Matrix* Matrix_multiply(Matrix* A, Matrix* B, f_matrix_multiply f) {
    if(A->n != B->m) {
        return NULL;
    }

    Matrix* C = Matrix_new(A->m, B->n);

    f(A->data, B->data, C->data, A->m, A->n, B->n);

    return C;
}

unsigned int Matrix_countzero(Matrix* M, f_matrix_countzero f) {

    unsigned int Z;

    f(M->data, M->m, M->n, &Z);

    return Z;
}

Matrix* Matrix_diff(Matrix* A, Matrix* B) {
    if(A->n != B->n) {
        return NULL;
    }
    if(A->m != B->m) {
        return NULL;
    }
    Matrix* C = Matrix_new(A->m, A->n);
    for(unsigned int i = A->m * A->n; i--;) {
        C->data[i] = A->data[i] - B->data[i];
    }
    return C;
}

void Matrix_print_listnonzero(Matrix* M, FILE* printFile) {
    for(unsigned int i = 0; i < M->m; ++i) {
        for(unsigned int j = 0; j < M->n; ++j) {
            float v = M->data[i * M->n + j];
            if(v != 0.0f) {
                fprintf(printFile, "%u,%u: %f\n", i, j, v);
            }
        }
    }
}

void Matrix_print(Matrix* M, FILE* printFile) {
    fprintf(printFile, "\n");

    int first = 1;

    if(M) {

        if(M->n > 128) {
            fprintf(printFile, "[ ... lots of numbers ... ] %ix%i\n", M->m, M->n);
        } else {

            float highestNumber = 0;
            for(unsigned int i = M->n * M->m; i--;) {
                int n = abs(M->data[i]);
                highestNumber = abs(highestNumber) < n ? M->data[i] : highestNumber;
            }
            int charsNeeded = highestNumber ? 1 + log10(highestNumber) : 1;

            unsigned int breakAt = 3;

            fprintf(printFile, "[");
            unsigned int i = 0;
            for(i = 0; i < M->m; ++i) {
                if(breakAt == i && M->n > 32) break;
                if(!first) fprintf(printFile, "\n ");
                first = 0;
                for(unsigned int j = 0; j < M->n; ++j) {
                    fprintf(printFile, " %*.0f", charsNeeded, M->data[i * M->n + j]);
                }
            }
            if(i < M->m) {
            fprintf(printFile, "\n   ... lots of numbers ...");
            }
            fprintf(printFile, " ] %ix%i\n", M->m, M->n);
        }
    } else {
        fprintf(printFile, "\n[null]\n");
    }
    fflush(printFile);
}

int Matrix_compare(Matrix* A, Matrix* B) {
    if(A->m < B->m) return -1;
    if(A->m > B->m) return 1;
    if(A->n < B->n) return -1;
    if(A->n > B->n) return 1;
    int e = A->n * A->m;
    for(int i = 0; i < e; ++i) {
        if(A->data[i] < B->data[i]) return -1;
        if(A->data[i] > B->data[i]) return 1;
    }
    return 0;
}

int Matrix_print_diff(Matrix* A, Matrix* B, FILE* printFile) {
    if(A->m < B->m) return -1;
    if(A->m > B->m) return 1;
    if(A->n < B->n) return -1;
    if(A->n > B->n) return 1;
    int e = A->n * A->m;
    int max_printed = 10;
    int different = 0;
    for(int i = 0; i < e; ++i) {
        if(A->data[i] != B->data[i]) {
            if(different < max_printed) {
                fprintf(printFile, "%i, %i: left:%f right:%f diff:%f\n", i / A->n, i % A->n, A->data[i], B->data[i], A->data[i] - B->data[i]);
            }
            ++different;
        }
    }
    fprintf(printFile, "Total differences: %i\n", different);
    return 0;
}

void Matrix_save(Matrix* M, FILE* file) {
    fwrite(&M->m, sizeof(M->m), 1, file);
    fwrite(&M->n, sizeof(M->n), 1, file);
    fwrite(M->data, sizeof(float), M->m * M->n, file);
}

Matrix* Matrix_load(FILE* file) {
    unsigned int m, n;
    fread(&m, sizeof(m), 1, file);
    fread(&n, sizeof(n), 1, file);
    Matrix* M = Matrix_new(m, n);
    fread(M->data, sizeof(float), M->m * M->n, file);
    return M;
}
