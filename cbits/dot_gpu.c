#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
 
#define MAX_SOURCE_SIZE (0x100000)

struct MatrixMultContext {
        cl_command_queue command_queue;
        cl_kernel kernel;

        cl_mem a_mem_obj;
        cl_mem b_mem_obj;
        cl_mem c_mem_obj;
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
                cl_int ret = clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
                ret = clGetDeviceIDs( platform_id, CL_DEVICE_TYPE_DEFAULT, 1, 
                        &device_id, &ret_num_devices);
                // Create an OpenCL context
                cl_context context = clCreateContext( NULL, 1, &device_id, NULL, NULL, &ret);
                // Create a command queue
                cl_command_queue command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
                // Create memory buffers on the device for each vector 
                cl_mem a_mem_obj = clCreateBuffer(context, CL_MEM_READ_ONLY, 
                        LIST_SIZE * sizeof(double), NULL, &ret);
                cl_mem b_mem_obj = clCreateBuffer(context, CL_MEM_READ_ONLY,
                        LIST_SIZE * sizeof(double), NULL, &ret);
                cl_mem c_mem_obj = clCreateBuffer(context, CL_MEM_WRITE_ONLY, 
                        LIST_SIZE * sizeof(double), NULL, &ret);
                // Create a program from the kernel source
                cl_program program = clCreateProgramWithSource(context, 1, 
                        (const char **)&source_str, (const size_t *)&source_size, &ret);
                // Build the program
                ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
                // Create the OpenCL kernel
                cl_kernel kernel = clCreateKernel(program, "vector_add", &ret);
 
                // Set structure fields
                
                mmc = malloc(sizeof(struct MatrixMultContext));
                mmc->command_queue = command_queue;
                mmc->kernel = kernel;
                mmc->a_mem_obj = a_mem_obj;
                mmc->b_mem_obj = b_mem_obj;
                mmc->c_mem_obj = c_mem_obj;
                printf("MMC singleton created!\n"); fflush(stdout);
        }
        return mmc;
}

double* dot(double* A, double* B) {
    // Set necessary parameters for convenience
    cl_command_queue command_queue; command_queue = getInstance()->command_queue;
    cl_kernel kernel; kernel = getInstance()->kernel;
    cl_mem a_mem_obj; a_mem_obj = getInstance()->a_mem_obj;
    cl_mem b_mem_obj; b_mem_obj = getInstance()->b_mem_obj;
    cl_mem c_mem_obj; c_mem_obj = getInstance()->c_mem_obj;

    // Copy the lists A and B to their respective memory buffers
    clEnqueueWriteBuffer(command_queue, a_mem_obj, CL_TRUE, 0,
            LIST_SIZE * sizeof(double), A, 0, NULL, NULL);
    clEnqueueWriteBuffer(command_queue, b_mem_obj, CL_TRUE, 0, 
            LIST_SIZE * sizeof(double), B, 0, NULL, NULL);
    // Set the arguments of the kernel
    clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&(a_mem_obj));
    clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&(b_mem_obj));
    clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&(c_mem_obj));
    // Execute the OpenCL kernel on the list
    size_t global_item_size = LIST_SIZE; // Process the entire lists
    size_t local_item_size = 64; // Divide work items into groups of 64
    clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, 
            &global_item_size, &local_item_size, 0, NULL, NULL);
    // Read the memory buffer C on the device to the local variable C
    double *C = (double*)malloc(sizeof(double)*LIST_SIZE);
    clEnqueueReadBuffer(command_queue, c_mem_obj, CL_TRUE, 0, 
            LIST_SIZE * sizeof(double), C, 0, NULL, NULL);
    return C;
}

// int main(void) {
//     // Create the two input vectors
//     int i;
//     double *A = (double*)malloc(sizeof(double)*LIST_SIZE);
//     double *B = (double*)malloc(sizeof(double)*LIST_SIZE);
//     for(i = 0; i < LIST_SIZE; i++) {
//         A[i] = i;
//         B[i] = LIST_SIZE - i;
//     }

//     double* C = NULL;
//     for(int i = 0; i < 10; ++i) {
//             for(int j = 0; j < 600; j++) {
//                 C = dot(A, B);
//                 // if(i == 0 && j == 0) {
//                 //         for(int k = 0; k < LIST_SIZE; k++) {
//                 //                 printf("%f + %f = %f\n", A[k], B[k], C[k]); fflush(stdout); 
//                 //         }
//                 // }
//             }
//     }
    
//     // Clean up
//     free(A);
//     free(B);
//     free(C);
//     return 0;
// }
