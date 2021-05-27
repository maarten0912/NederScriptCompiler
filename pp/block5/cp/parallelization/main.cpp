#include <stdio.h>
#include <tests.h>
#include <kernels.h>

void run_tests_matrix_multiply(FILE* out) {
    fprintf(out, "Testing matrix multiply (sequential)...\n");
    test_multiply(out, impl_matrix_multiply_sequential);

    fprintf(out, "Testing matrix multiply (openmp)...\n");
    test_multiply(out, impl_matrix_multiply_openmp);

    fprintf(out, "\n");
}

void run_tests_count_zero(FILE* out) {
    fprintf(out, "Testing count zero (sequential)...\n");
    test_countzero(out, impl_matrix_countzero_sequential);

    fprintf(out, "Testing count zero (openmp)...\n");
    test_countzero(out, impl_matrix_countzero_openmp);

    fprintf(out, "Testing count zero (opencl)...\n");
    test_countzero(out, impl_matrix_countzero_opencl);

    fprintf(out, "\n");
}

void run_tests_vector_sort(FILE* out) {
    fprintf(out, "Testing vector sort (sequential)...\n");
    test_vectorsort(out, impl_vector_sort_sequential);

    fprintf(out, "Testing vector sort (openmp)...\n");
    test_vectorsort(out, impl_vector_sort_openmp);
    
    fprintf(out, "\n");
}

void run_tests_vector_sum(FILE* out) {
    fprintf(out, "Testing vector sum (sequential)...\n");
    test_vectorsum(out, impl_vector_sum_sequential);

    fprintf(out, "Testing vector sum (opencl)...\n");
    test_vectorsum(out, impl_vector_sum_opencl);
}

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;

    FILE* out = stdout;
    run_tests_matrix_multiply(out); // Ex 1
    run_tests_count_zero(out); // Ex 2 + 4
    run_tests_vector_sort(out); // Ex 3
    run_tests_vector_sum(out); // Ex 5

    return 0;
}


