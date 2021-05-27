#define _POSIX_C_SOURCE 200112L

#include <libgen.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <algorithm>

#include <tests.h>
#include <vector.h>
#include <matrix.h>
#include <kernels.h>

#if defined(CLOCK_MONOTONIC_RAW)
#  define USE_CLOCK CLOCK_MONOTONIC_RAW
#elif defined(CLOCK_MONOTONIC)
#  define USE_CLOCK CLOCK_MONOTONIC
#else
#  error "No clock!"
#endif


#define ASSERT(c,s) do { \
    if(!(c)) { \
        fprintf(printFile, s "\n"); \
        return 1; \
    } \
} while(0)

TestResult test_kernel_multiply(FILE* printFile, Matrix* A, Matrix* B, Matrix* CC, f_matrix_multiply kernel) {
    TestResult tr;
    Timer t = Timer_start();
    Matrix* C = Matrix_multiply(A, B, kernel);
    tr.time = Timer_passed(t);

    tr.correct = Matrix_compare(C, CC);

    if(tr.correct) {
        fprintf(printFile, "Multiplication failed.\n");
        fprintf(printFile, "A = ");
        Matrix_print(A, printFile);
        fprintf(printFile, "B = ");
        Matrix_print(B, printFile);
        fprintf(printFile, "C = ");
        Matrix_print(C, printFile);
        fprintf(printFile, "Expected:");
        Matrix_print(CC, printFile);
        fprintf(printFile, "Differences between expected (left) and C (right):\n");
        Matrix_print_diff(CC, C, printFile);

    }

    Matrix_delete(C);
    return tr;
}

TestResult test_kernel_countzero(FILE* printFile, Matrix* M, unsigned int CZ, f_matrix_countzero kernel) {
    TestResult tr;
    Timer t = Timer_start();
    unsigned int Z = Matrix_countzero(M, kernel);
    tr.time = Timer_passed(t);
    tr.correct = Z != CZ;

    if(tr.correct) {
        fprintf(printFile, "Count zero failed.\n");
        fprintf(printFile, "M = ");
        Matrix_print(M, printFile);
        fprintf(printFile, "Z = %u\n", Z);
        fprintf(printFile, "Expected: %u\n", CZ);

    }

    return tr;
}

TestResult test_kernel_vectorsort(FILE* printFile, Vector* V, Vector* CV, f_vector_sort kernel) {
    TestResult tr;
    Timer t = Timer_start();
    Vector* C = Vector_copy(V);
    Vector_sort(V, kernel);
    tr.time = Timer_passed(t);
    tr.correct = Vector_compare(V, CV);

    if(tr.correct) {
        fprintf(printFile, "Vector sort failed.\n");
        fprintf(printFile, "V = ");
        Vector_print(V, printFile);
        fprintf(printFile, "C = ");
        Vector_print(C, printFile);
        fprintf(printFile, "Expected:");
        Vector_print(CV, printFile);

    }

    Vector_delete(C);

    return tr;
}

TestResult test_kernel_vectorsum(FILE* printFile, Vector* V, float CS, f_vector_sum kernel) {
    TestResult tr;
    Timer t = Timer_start();
    float S = Vector_sum(V, kernel);
    tr.time = Timer_passed(t);
    tr.correct = S != CS;

    if(tr.correct) {
        fprintf(printFile, "Vector sum failed.\n");
        fprintf(printFile, "V = ");
        Vector_print(V, printFile);
        fprintf(printFile, "S = %f\n", S);
        fprintf(printFile, "Expected: %f\n", CS);
    }

    return tr;
}

TestResult test1(FILE* printFile, f_matrix_multiply kernel) {
    (void)printFile;

    float a[] = {3};
    float b[] = {6};
    float c_correct[] = {18};

    Matrix* A = Matrix_create(1, 1, a);
    Matrix* B = Matrix_create(1, 1, b);
    Matrix* CC = Matrix_create(1, 1, c_correct);
    TestResult tr = test_kernel_multiply(printFile, A, B, CC, kernel);
    Matrix_delete(A);
    Matrix_delete(B);
    Matrix_delete(CC);

    return tr;
}

TestResult test2(FILE* printFile, f_matrix_multiply kernel) {
    (void)printFile;

    float a[] = {1,2,3,4,5,6};
    float b[] = {1,2,1,2,1,2};
    float c_correct[] = {6,12,15,30};

    Matrix* A = Matrix_create(2, 3, a);
    Matrix* B = Matrix_create(3, 2, b);
    Matrix* CC = Matrix_create(2, 2, c_correct);
    TestResult tr = test_kernel_multiply(printFile, A, B, CC, kernel);
    Matrix_delete(A);
    Matrix_delete(B);
    Matrix_delete(CC);

    return tr;
}

TestResult test3(FILE* printFile, f_matrix_multiply kernel) {
    (void)printFile;

    float a[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    float b[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    float c_correct[] = { 90, 100, 110, 120
                      ,202, 228, 254, 280
                      ,314, 356, 398, 440
                      ,426, 484, 542, 600};

    Matrix* A = Matrix_create(4, 4, a);
    Matrix* B = Matrix_create(4, 4, b);
    Matrix* CC = Matrix_create(4, 4, c_correct);
    TestResult tr = test_kernel_multiply(printFile, A, B, CC, kernel);
    Matrix_delete(A);
    Matrix_delete(B);
    Matrix_delete(CC);

    return tr;
}

Matrix* test_mul_create_or_load(Matrix* A, Matrix* B) {
    char buffer[512];
    int written = readlink("/proc/self/exe", buffer, 512);
    char* cp = buffer + written;
    while(*cp != '/') cp--;
    *cp = 0;
    strcat(cp, "/matrices/");
    mkdir(buffer, S_IRWXU | S_IRWXG | S_IRWXO);
    snprintf(cp, 64, "/matrices/mat%ux%ux%u", A->m, A->n, B->n);
    FILE* file = fopen(buffer, "r");
    if(file) {
        Matrix* M = Matrix_load(file);
        fclose(file);
        return M;
    } else {
        FILE* file = fopen(buffer, "w+");
        Matrix* M = Matrix_multiply(A, B, impl_matrix_multiply_sequential);
        if(file) {
            Matrix_save(M, file);
            fclose(file);
        } else {
            printf("ERROR! could not write matrix\n");
        }
        return M;
    }
}

TestResult test_n(FILE* printFile, f_matrix_multiply kernel, int n) {
    (void)printFile;
    TestResult tr;

    float* a = (float*)malloc(sizeof(float) * n * n);
    float* b = (float*)malloc(sizeof(float) * n * n);

    for(int i=n*n; i--;) {
        a[i] = (7 * i) % 19;
        b[i] = 19 - ((9 * i) % 19);
    }

    Matrix* A = Matrix_create(n, n, a);
    Matrix* B = Matrix_create(n, n, b);
    //Matrix* CC = Matrix_multiply(A, B, impl_matrix_multiply_sequential);
    Matrix* CC = test_mul_create_or_load(A, B);
    tr = test_kernel_multiply(printFile, A, B, CC, kernel);
    Matrix_delete(A);
    Matrix_delete(B);
    Matrix_delete(CC);

    free(a);
    free(b);

    return tr;
}

TestResult test_n(FILE* printFile, f_matrix_countzero kernel, int n) {
    (void)printFile;
    TestResult tr;

    float* a = (float*)malloc(sizeof(float) * n * n);

    for(int i=n*n; i--;) {
        a[i] = (7 * i) % 19;
    }

    Matrix* M = Matrix_create(n, n, a);
    //Matrix* CC = Matrix_multiply(A, B, impl_matrix_multiply_sequential);
    unsigned int Z = Matrix_countzero(M, impl_matrix_countzero_sequential);
    tr = test_kernel_countzero(printFile, M, Z, kernel);
    Matrix_delete(M);

    free(a);

    return tr;
}

TestResult test_n(FILE* printFile, f_vector_sort kernel, int n) {
    (void)printFile;
    TestResult tr;

    Vector* V = Vector_new(n);

    for(int i=n; i--;) {
        V->data[i] = 199 - (7 * i) % 199;
    }

    Vector* CV = Vector_copy(V);
    std::sort(CV->data, CV->data+CV->n);

    tr = test_kernel_vectorsort(printFile, V, CV, kernel);

    Vector_delete(CV);
    Vector_delete(V);

    return tr;
}

TestResult test_n(FILE* printFile, f_vector_sum kernel, int n) {
    (void)printFile;
    TestResult tr;

    Vector* V = Vector_new(n);

//    int max = 256;
//
//    for(int i=n; i--;) {
//        V->data[i] = ((float)(i%max))/16.0f;
//    }
//
//    float CS = ((float)n)*(((float)n)-1)/32.0f;
//
//    if(n > max) {
//        CS = ((float)(n/max)) * ((float)max)*(((float)max)-1)/32.0f;
//        CS += ((float)(n%max))*(((float)(n%max))-1)/32.0f;
//    }

    for(int i=n; i--;) {
        V->data[i] = 1.0f;
    }
    float CS = Vector_sum(V, impl_vector_sum_sequential);

    tr = test_kernel_vectorsum(printFile, V, CS, kernel);

    Vector_delete(V);

    return tr;
}

template<typename KERNEL>
void test(FILE* printFile, int* sizes, int max, KERNEL kernel) {
    int lmax = 1 + log10(max);
    for(int i = 0; i < max; ++i) {
        int s = sizes[i];
        TestResult tr = test_n(printFile, kernel, s);
        fprintf(printFile, "[%*i/%i] %i:", lmax, i+1, max, s);
        if(tr.correct) {
            fprintf(printFile, "FAIL\n");
        } else if(tr.time == 0.0) {
            fprintf(printFile, "PASS \n");
        } else {
            fprintf(printFile, "PASS, took %.1lf ms\n", tr.time);
        }
        fflush(printFile);
    }
}

void test_multiply(FILE* printFile, f_matrix_multiply kernel) {

    int sizes[] = {1,2,4,8,16,32,64,128,256,512,1024};
    int max = sizeof(sizes)/sizeof(*sizes);
    test(printFile, sizes, max, kernel);
}

void test_countzero(FILE* printFile, f_matrix_countzero kernel) {

    int sizes[] = {1,2,4,8,16,32,64,128,256,512,1024, 2048, 4096, 8192, 16384};
    int max = sizeof(sizes)/sizeof(*sizes);
    test(printFile, sizes, max, kernel);
}

void test_vectorsort(FILE* printFile, f_vector_sort kernel) {

    int sizes[] = {1,2,4,8,16,32,64,128,256,512,1024, 2048, 4096, 8192, 16384, 32768};
    int max = sizeof(sizes)/sizeof(*sizes);
    test(printFile, sizes, max, kernel);
}

void test_vectorsum(FILE* printFile, f_vector_sum kernel) {

    int sizes[] = {1,2,4,8,16,32,64,128,256,512,1024, 2048, 4096, 8192, 16384, 32768};
    int max = sizeof(sizes)/sizeof(*sizes);
    test(printFile, sizes, max, kernel);
}

Timer Timer_start() {
    Timer t;
    clock_gettime(USE_CLOCK, &t.start);
    return t;
}

double Timer_passed(Timer t) {
    struct timespec e;
    clock_gettime(USE_CLOCK, &e);
    double s = (e.tv_nsec - t.start.tv_nsec) * 0.000001 + (e.tv_sec - t.start.tv_sec) * 1000.0;
    return s;
}
