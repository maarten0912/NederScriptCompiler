#include <stdio.h>
#ifdef __APPLE__
#  include "OpenCL/opencl.h"
#else
#  include "CL/cl.h"
#endif

#define KERNEL(x) #x

// A is Am * An matrix, B is array of length Bn
const char* kernelstring = KERNEL(
__kernel void matrix_multiply
( const int M
, const int N
, const int K
, const __global float* A
, const __global float* B
, __global float* C
) {

    // You do not have to implement this

}

/*
Counts the number of elements with value of 0 
in the array one-dimensional array M. 
Final value should be stored in count, and each
thread should process Mn elements. 
*/
__kernel void countzero
( const int Mn
, const __global float* M
, volatile __global int* count
) {

    // Implement this for exercise 5-CP.4

}


/* 
Computes sum of all elements in array V. 
V is a single-dimensional array of total length Vn.
The result should be stored in the FIRST position 
of V (so index 0).  
*/
__kernel void vector_sum
( int Vn
, __global float* V
) {

    // Implement this for exercise 5-CP.5

}

__kernel void vectoradd
( __global const int* coins
, __global int* solution
, int nr_coins
, int amount
) {

    // Implement this for exercise 5-CP.6

}
);

const char *getErrorString(cl_int error)
{
switch(error){
    // run-time and JIT compiler errors
    case 0: return "CL_SUCCESS";
    case -1: return "CL_DEVICE_NOT_FOUND";
    case -2: return "CL_DEVICE_NOT_AVAILABLE";
    case -3: return "CL_COMPILER_NOT_AVAILABLE";
    case -4: return "CL_MEM_OBJECT_ALLOCATION_FAILURE";
    case -5: return "CL_OUT_OF_RESOURCES";
    case -6: return "CL_OUT_OF_HOST_MEMORY";
    case -7: return "CL_PROFILING_INFO_NOT_AVAILABLE";
    case -8: return "CL_MEM_COPY_OVERLAP";
    case -9: return "CL_IMAGE_FORMAT_MISMATCH";
    case -10: return "CL_IMAGE_FORMAT_NOT_SUPPORTED";
    case -11: return "CL_BUILD_PROGRAM_FAILURE";
    case -12: return "CL_MAP_FAILURE";
    case -13: return "CL_MISALIGNED_SUB_BUFFER_OFFSET";
    case -14: return "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST";
    case -15: return "CL_COMPILE_PROGRAM_FAILURE";
    case -16: return "CL_LINKER_NOT_AVAILABLE";
    case -17: return "CL_LINK_PROGRAM_FAILURE";
    case -18: return "CL_DEVICE_PARTITION_FAILED";
    case -19: return "CL_KERNEL_ARG_INFO_NOT_AVAILABLE";

    // compile-time errors
    case -30: return "CL_INVALID_VALUE";
    case -31: return "CL_INVALID_DEVICE_TYPE";
    case -32: return "CL_INVALID_PLATFORM";
    case -33: return "CL_INVALID_DEVICE";
    case -34: return "CL_INVALID_CONTEXT";
    case -35: return "CL_INVALID_QUEUE_PROPERTIES";
    case -36: return "CL_INVALID_COMMAND_QUEUE";
    case -37: return "CL_INVALID_HOST_PTR";
    case -38: return "CL_INVALID_MEM_OBJECT";
    case -39: return "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
    case -40: return "CL_INVALID_IMAGE_SIZE";
    case -41: return "CL_INVALID_SAMPLER";
    case -42: return "CL_INVALID_BINARY";
    case -43: return "CL_INVALID_BUILD_OPTIONS";
    case -44: return "CL_INVALID_PROGRAM";
    case -45: return "CL_INVALID_PROGRAM_EXECUTABLE";
    case -46: return "CL_INVALID_KERNEL_NAME";
    case -47: return "CL_INVALID_KERNEL_DEFINITION";
    case -48: return "CL_INVALID_KERNEL";
    case -49: return "CL_INVALID_ARG_INDEX";
    case -50: return "CL_INVALID_ARG_VALUE";
    case -51: return "CL_INVALID_ARG_SIZE";
    case -52: return "CL_INVALID_KERNEL_ARGS";
    case -53: return "CL_INVALID_WORK_DIMENSION";
    case -54: return "CL_INVALID_WORK_GROUP_SIZE";
    case -55: return "CL_INVALID_WORK_ITEM_SIZE";
    case -56: return "CL_INVALID_GLOBAL_OFFSET";
    case -57: return "CL_INVALID_EVENT_WAIT_LIST";
    case -58: return "CL_INVALID_EVENT";
    case -59: return "CL_INVALID_OPERATION";
    case -60: return "CL_INVALID_GL_OBJECT";
    case -61: return "CL_INVALID_BUFFER_SIZE";
    case -62: return "CL_INVALID_MIP_LEVEL";
    case -63: return "CL_INVALID_GLOBAL_WORK_SIZE";
    case -64: return "CL_INVALID_PROPERTY";
    case -65: return "CL_INVALID_IMAGE_DESCRIPTOR";
    case -66: return "CL_INVALID_COMPILER_OPTIONS";
    case -67: return "CL_INVALID_LINKER_OPTIONS";
    case -68: return "CL_INVALID_DEVICE_PARTITION_COUNT";

    // extension errors
    case -1000: return "CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR";
    case -1001: return "CL_PLATFORM_NOT_FOUND_KHR";
    case -1002: return "CL_INVALID_D3D10_DEVICE_KHR";
    case -1003: return "CL_INVALID_D3D10_RESOURCE_KHR";
    case -1004: return "CL_D3D10_RESOURCE_ALREADY_ACQUIRED_KHR";
    case -1005: return "CL_D3D10_RESOURCE_NOT_ACQUIRED_KHR";
    default: return "Unknown OpenCL error";
    }
}

cl_context context;
cl_command_queue queue;
cl_program program;

void unprepare() {
    // Clean-up OpenCL
    clReleaseCommandQueue(queue);
    clReleaseContext(context);
    clReleaseProgram(program);
}

void prepare() {

    static int setup = 0;
    if(setup) return;
    setup = 1;

    // Opencl Setup
    cl_platform_id platform = 0;
    clGetPlatformIDs(1, &platform, NULL);
    cl_device_id device = 0;
    clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 1, &device, NULL);
    context = clCreateContext(NULL, 1, &device, NULL, NULL, NULL);
    queue = clCreateCommandQueue(context, device, 0, NULL);
    char deviceName[1024];
    clGetDeviceInfo(device, CL_DEVICE_NAME, 1024, deviceName, NULL);

    // Compile the kernel
    program = clCreateProgramWithSource(context, 1, &kernelstring, NULL, NULL);
    clBuildProgram(program, 0, NULL, "", NULL, NULL);

    // Check for compilation errors
    size_t logSize;
    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &logSize);
    char* messages = (char*)malloc((1+logSize)*sizeof(char));
    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, logSize, messages, NULL);
    messages[logSize] = '\0';
    if (logSize > 10) { printf(">>> Compiler message: %s\n", messages); }
    free(messages);

    atexit(unprepare);
}

void impl_matrix_multiply_opencl( float* A
                                , float* B
                                , float* C
                                , unsigned int Am
                                , unsigned int An
                                , unsigned int Bn
                                ) {
    unsigned int M = Am;
    unsigned int K = An;
    unsigned int N = Bn;
    cl_event event = NULL;

    // Prepare program
    prepare();

    // Prepare OpenCL memory objects
    cl_mem bufA = clCreateBuffer(context, CL_MEM_READ_ONLY,  M*K*sizeof(float), NULL, NULL);
    cl_mem bufB = clCreateBuffer(context, CL_MEM_READ_ONLY,  K*N*sizeof(float), NULL, NULL);
    cl_mem bufC = clCreateBuffer(context, CL_MEM_READ_WRITE, M*N*sizeof(float), NULL, NULL);

    // Copy matrices to the GPU
    clEnqueueWriteBuffer(queue, bufA, CL_TRUE, 0, M*K*sizeof(float), A, 0, NULL, NULL);
    clEnqueueWriteBuffer(queue, bufB, CL_TRUE, 0, K*N*sizeof(float), B, 0, NULL, NULL);
    clEnqueueWriteBuffer(queue, bufC, CL_TRUE, 0, M*N*sizeof(float), C, 0, NULL, NULL);

    // Setup call to the opencl kernel
    cl_int err = CL_SUCCESS;
    cl_kernel kernel = clCreateKernel(program, "matrix_multiply", &err);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 0, sizeof(unsigned int), (void*)&M);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 1, sizeof(unsigned int), (void*)&N);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 2, sizeof(unsigned int), (void*)&K);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 3, sizeof(cl_mem), (void*)&bufA);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 4, sizeof(cl_mem), (void*)&bufB);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 5, sizeof(cl_mem), (void*)&bufC);

    // Call the opencl kernel for every element of the C matrix
    size_t local[2] = { M/16, N/16 };
    const size_t global[2] = { M, N };
    if(local[0] == 0) local[0] = M;
    if(local[1] == 0) local[1] = N;
    err = clEnqueueNDRangeKernel(queue, kernel, 2, NULL,
                                 global, NULL, 0, NULL, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clWaitForEvents(1, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Copy back the resulting C matrix to host memory
    clEnqueueReadBuffer(queue, bufC, CL_TRUE, 0, M * N * sizeof(float), C, 0, NULL, &event);
    err = clWaitForEvents(1, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Free the OpenCL memory objects
    clReleaseMemObject(bufA);
    clReleaseMemObject(bufB);
    clReleaseMemObject(bufC);

    clReleaseKernel(kernel);

}

void impl_matrix_countzero_opencl( float* M
                                 , unsigned int Mm
                                 , unsigned int Mn
                                 , unsigned int* Z
                                 ) {

    cl_int err = CL_SUCCESS;
    cl_event event = NULL;

    // Prepare program
    prepare();

    // Prepare OpenCL memory objects
    cl_mem bufA = clCreateBuffer(context, CL_MEM_READ_ONLY,  Mm*Mn*sizeof(float), NULL, NULL);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    cl_mem bufC = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    int count = 0;

    // Copy matrices to the GPU
    clEnqueueWriteBuffer(queue, bufA, CL_TRUE, 0, Mm*Mn*sizeof(float), M, 0, NULL, NULL);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    clEnqueueWriteBuffer(queue, bufC, CL_TRUE, 0, sizeof(int), &count, 0, NULL, NULL);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Setup call to the opencl kernel
    cl_kernel kernel = clCreateKernel(program, "countzero", &err);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 0, sizeof(unsigned int), (void*)&Mn);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void*)&bufA);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void*)&bufC);

    // Call the opencl kernel for every row of the M matrix
    size_t local[1] = { Mm/16 };
    const size_t global[1] = { Mm };
    if(local[0] == 0) local[0] = Mm;
    err = clEnqueueNDRangeKernel(queue, kernel, 1, NULL,
                                 global, NULL, 0, NULL, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clWaitForEvents(1, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Copy back the resulting C matrix to host memory
    clEnqueueReadBuffer(queue, bufC, CL_TRUE, 0, sizeof(int), &count, 0, NULL, &event);
    err = clWaitForEvents(1, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Free the OpenCL memory objects
    clReleaseMemObject(bufA);
    clReleaseMemObject(bufC);

    clReleaseKernel(kernel);

    *Z = count;
}

void impl_vector_sum_opencl( float* V
                           , unsigned int Vn
                           , float* S
                           ) {

    cl_int err = CL_SUCCESS;
    cl_event event = NULL;

    // Prepare program
    prepare();

    // Prepare OpenCL memory objects
    cl_mem bufA = clCreateBuffer(context, CL_MEM_READ_WRITE,  Vn*sizeof(float), NULL, NULL);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Copy matrices to the GPU
    clEnqueueWriteBuffer(queue, bufA, CL_TRUE, 0, Vn*sizeof(float), V, 0, NULL, NULL);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Setup call to the opencl kernel
    cl_kernel kernel = clCreateKernel(program, "sum", &err);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 0, sizeof(unsigned int), (void*)&Vn);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void*)&bufA);

    // Call the opencl kernel for every element of the V vector
    size_t global[1] = { Vn };
    if(global[0] == 0) global[0] = Vn;
    err = clEnqueueNDRangeKernel(queue, kernel, 1, NULL,
                                 global, NULL, 0, NULL, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));
    err = clWaitForEvents(1, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Copy back the resulting sum to host memory
    float sum = 0;
    clEnqueueReadBuffer(queue, bufA, CL_TRUE, 0, sizeof(float), &sum, 0, NULL, &event);
    err = clWaitForEvents(1, &event);
    if(err != CL_SUCCESS)    printf("%i: %s\n", __LINE__, getErrorString(err));

    // Free the OpenCL memory objects
    clReleaseMemObject(bufA);

    clReleaseKernel(kernel);

    *S = sum;
}
