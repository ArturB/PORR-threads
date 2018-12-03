__kernel void
matrixMul(__global double* result, 
          __global double* A, 
          __global double* B,
          int wide)
{
  
   // value stores the element that is 
   // computed by the thread
   double value = 0;
   for (int k = 0; k < wide; ++k)
   {
      value += A[k] * B[k];
   }
 
   // Write the result to device memory each 
   result[0] = value;
}