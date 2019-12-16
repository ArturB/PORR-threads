#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
 
#define MAX_SOURCE_SIZE (0x100000)

struct MatrixMultContext {
        cl_context context; 
        cl_command_queue command_queue;
        cl_kernel kernel;
};

const int LIST_SIZE = 240000;
struct MatrixMultContext* mmc = NULL;


struct MatrixMultContext* getInstance() {
        if (mmc == NULL) {
                printf("Creating the mmc first time...\n"); fflush(stdout);
                // Load the kernel source code into the array source_str
                FILE *fp;
                char *source_str;
                size_t source_size;
                fp = fopen("dot_gpu.cl", "r");
                if (!fp) {
                        fprintf(stderr, "Failed to load kernel.\n");
                        exit(1);
                }
                source_str = (char*)malloc(MAX_SOURCE_SIZE);
                source_size = fread( source_str, 1, MAX_SOURCE_SIZE, fp);
                fclose( fp );
                // Get platform and device information
                cl_platform_id platform_id = NULL;
                cl_device_id device_id = NULL;   
                cl_uint ret_num_devices;
                cl_uint ret_num_platforms;
                clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
                clGetDeviceIDs( platform_id, CL_DEVICE_TYPE_DEFAULT, 1, 
                        &device_id, &ret_num_devices);
                // Create an OpenCL context
                cl_context context = clCreateContext( NULL, 1, &device_id, NULL, NULL, NULL);
                // Create a command queue
                cl_command_queue command_queue = clCreateCommandQueue(context, device_id, 0, NULL);
                // Create a program from the kernel source
                cl_program program = clCreateProgramWithSource(context, 1, 
                        (const char **)&source_str, (const size_t *)&source_size, NULL);
                // Build the program
                clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
                // Create the OpenCL kernel
                cl_kernel kernel = clCreateKernel(program, "matrixMult", NULL);
 
                // Set structure fields
                
                mmc = malloc(sizeof(struct MatrixMultContext));
                mmc->context = context;
                mmc->command_queue = command_queue;
                mmc->kernel = kernel;
                printf("MMC singleton created!\n"); fflush(stdout);
        }
        return mmc;
}

double* CL_CALL(double* A, double* B, int rows1, int cols1, int rows2, int cols2) {
    // Set necessary parameters for convenience
    cl_context context = getInstance()->context;
    cl_command_queue command_queue = getInstance()->command_queue;
    cl_kernel kernel = getInstance()->kernel;

    // Create memory buffers on the device for each matrix 
    cl_mem a_mem_obj = clCreateBuffer(context, CL_MEM_READ_ONLY, 
        rows1 * cols1 * sizeof(double), NULL, NULL);
    cl_mem b_mem_obj = clCreateBuffer(context, CL_MEM_READ_ONLY,
        rows2 * cols2 * sizeof(double), NULL, NULL);
    cl_mem c_mem_obj = clCreateBuffer(context, CL_MEM_WRITE_ONLY, 
        rows1 * cols2 * sizeof(double), NULL, NULL);
    // Copy the matrices A and B to their respective memory buffers
    clEnqueueWriteBuffer(command_queue, a_mem_obj, CL_TRUE, 0,
            rows1 * cols1 * sizeof(double), A, 0, NULL, NULL);
    clEnqueueWriteBuffer(command_queue, b_mem_obj, CL_TRUE, 0, 
            rows2 * cols2 * sizeof(double), B, 0, NULL, NULL);
    // Set the arguments of the kernel
    clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&(a_mem_obj));
    clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&(b_mem_obj));
    clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&(c_mem_obj));
    clSetKernelArg(kernel, 3, sizeof(int), (void *)&(rows1));
    clSetKernelArg(kernel, 4, sizeof(int), (void *)&(cols1));
    clSetKernelArg(kernel, 5, sizeof(int), (void *)&(rows2));
    clSetKernelArg(kernel, 6, sizeof(int), (void *)&(cols2));
    // Execute the OpenCL kernel on the matrices
    const size_t global_item_size[2] = { rows1, cols2 }; // Process the entire matrices
    const size_t local_item_size[2] = {1, 1}; // Divide work items into groups of 32
    clEnqueueNDRangeKernel(command_queue, kernel, 2, NULL, 
            global_item_size, local_item_size, 0, NULL, NULL);
    // Read the memory buffer C on the device to the local variable C
    double *C = (double*)malloc(sizeof(double)*rows1*cols2);
    clEnqueueReadBuffer(command_queue, c_mem_obj, CL_TRUE, 0, 
            rows1 * cols2 * sizeof(double), C, 0, NULL, NULL);
    return C;
}


double dot(double* v1, double* v2, int s) {
    double res = 0; 
    for(int i = 0; i < s; i++) {
        res = res + v1[i] * v2[i];
    }
    return res;
}

double** matrixDeserializeAsVector(double* m, int rows, int cols) {
    double** dm = malloc(rows * sizeof(double*));
    for(int i = 0; i < rows; ++i) {
        dm[i] = malloc(cols * sizeof(double));
        for(int j = 0; j < cols; ++j) {
            dm[i][j] = m[i * cols + j];
        }
    }
    return dm;
}

double** matrixDeserializeAsFunctional(double* m, int rows, int cols) {
    double** dm = malloc(cols * sizeof(double*));
    for(int i = 0; i < cols; ++i) {
        dm[i] = malloc(rows * sizeof(double));
        for(int j = 0; j < rows; ++j) {
            dm[i][j] = m[j * cols + i];
        }
    }
    return dm;
}

double* matrixMult(double* m1, double* m2, int rows1, int cols1, int rows2, int cols2) {
    if(cols1 != rows2) {
        printf("Incompatible matrix sizes!\n"); fflush(stdout);
        return NULL;
    }

    double** dm1 = matrixDeserializeAsVector(m1, rows1, cols1);
    double** dm2 = matrixDeserializeAsFunctional(m2, rows2, cols2);
    double*   m3 = malloc(rows1 * cols2 * sizeof(double));
    for(int i = 0; i < rows1; ++i) {
        for(int j = 0; j < cols2; ++j) {
            m3[i * cols2 + j] = dot(dm1[i], dm2[j], cols1);
        }
    }
    for(int i = 0; i < rows1; ++i) {
        free(dm1[i]);
    }
    for(int i = 0; i < cols2; ++i) {
        free(dm2[i]);
    }
    free(dm1); free(dm2);
    return m3;
}

void matrixPrintf(double** dm, int outerSize, int innerSize) {
    for(int i = 0; i < outerSize; ++i) {
        printf("[ "); 
        for(int j = 0; j < innerSize; ++j) {
            printf("%.1f ", dm[i][j]);
        }
        printf("]\n"); fflush(stdout);
    }
}

void matrixFreeMem(double** dm, int outerSize, int innerSize) {
    for(int i = 0; i < outerSize; ++i) {
        free(dm[i]);
    }
    free(dm);
}




// int main(void) {
//     // Create the two input vectors
//     int i;
//     double *A = (double*)malloc(sizeof(double)*3);
//     double *B = (double*)malloc(sizeof(double)*9);
//     for(i = 0; i < 3; i++) {
//         A[i] = i;
//     }
//     for(i = 0; i < 9; ++i) {
//         B[i] = i;
//     }

//     double* C = NULL;
//     for(int i = 0; i < 45; ++i) {
//             for(int j = 0; j < 20; j++) {
//                 C = CL_CALL(A, B, 1, 3, 3, 3); 
//                 if(i == 0 && j == 0) {
//                         double** dmC = matrixDeserializeAsVector(C,1,3);
//                         matrixPrintf(dmC, 1, 3);
//                         matrixFreeMem(dmC, 1, 3);
//                 }
//                 free(C);
//             }
//     }
    
//     // Clean up
//     free(A);
//     free(B);
//     return 0;
// }
