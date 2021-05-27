#include <vector.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define VECTOR_ELEMENT_T float

Vector* Vector_new(int n) {
    Vector* V = (Vector*)malloc(sizeof(Vector));
    V->n = n;
    V->data = (VECTOR_ELEMENT_T*)malloc(n*sizeof(VECTOR_ELEMENT_T));
    return V;
}

void Vector_delete(Vector* V) {
    if(V) {
        if(V->data) free(V->data);
        free(V);
    }
}

Vector* Vector_copy(Vector* V) {
    Vector* Vcopy = Vector_new(V->n);
    memcpy(Vcopy->data, V->data, V->n * sizeof(VECTOR_ELEMENT_T));
    return Vcopy;
}

void Vector_sort(Vector* V, f_vector_sort f) {
    f(V->data, V->n);
}

int Vector_compare(Vector* A, Vector* B) {
    if(A->n < B->n) return -1;
    if(A->n > B->n) return 1;
    int e = A->n;
    for(int i = 0; i < e; ++i) {
        if(A->data[i] < B->data[i]) return -1;
        if(A->data[i] > B->data[i]) return 1;
    }
    return 0;
}

void Vector_print(Vector* V, FILE* printFile) {
    fprintf(printFile, "\n");

    if(V) {

        if(V->n > 128) {
            fprintf(printFile, "[ ... lots of numbers ... ] %i\n", V->n);
        } else {

            fprintf(printFile, "[");
            unsigned int i = 0;
            for(i = 0; i < V->n; ++i) {
                fprintf(printFile, " %.0f", V->data[i]);
            }
            fprintf(printFile, " ] %i\n", V->n);
        }
    } else {
        fprintf(printFile, "\n[null]\n");
    }
    fflush(printFile);
}

float Vector_sum(Vector* V, f_vector_sum f) {
    float sum = 0;
    f(V->data, V->n, &sum);
    return sum;
}
