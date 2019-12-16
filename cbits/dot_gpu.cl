

__kernel void matrixMult(
    __global const double *A, 
    __global const double *B, 
    __global double *C, 
    int rows1, 
    int cols1, 
    int rows2, 
    int cols2) {
 
    // Thread identifiers
    const int globalRow = get_global_id(0); // Row ID of C (0..M)
    const int globalCol = get_global_id(1); // Col ID of C (0..N)
 
    // Compute a single element (loop over contracted index)
    double acc = 0.0;
    for (int k = 0; k < cols1; ++k) {
        acc += A[globalRow * cols1 + k] * B[k * cols2 + globalCol];
    }
 
    // Store the result
    C[globalRow * cols2 + globalCol] = acc;
}

