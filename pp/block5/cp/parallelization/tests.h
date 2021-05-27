#pragma once

#include <cstdio>
#include <ctime>
#include <utility>
#include <iostream>

#include <matrix.h>
#include <vector.h>
#include <kernels.h>

typedef struct {
    struct timespec start;
} Timer;

Timer Timer_start();
double Timer_passed(Timer t);

typedef struct {
    double time;
    int correct;
} TestResult;

void test_multiply(FILE* printFile, f_matrix_multiply kernel);
void test_countzero(FILE* printFile, f_matrix_countzero kernel);
void test_vectorsort(FILE* printFile, f_vector_sort kernel);
void test_vectorsum(FILE* printFile, f_vector_sum kernel);

typedef TestResult (*test_t)(FILE* , f_matrix_multiply kernel);

template<typename T>
struct Kernel {
    std::string name;
    T f;

    Kernel(std::string name, T f)
    :   name(std::move(name))
    ,   f(std::move(f))
    {
    }
};

