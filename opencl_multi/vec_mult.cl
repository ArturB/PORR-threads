__kernel void
matrixMul(__global double* result, 
          __global double* A, 
          __global double* B,
          const int wide)
{
  
   // value stores the element that is 
   // computed by the thread
   int id = get_global_id(0);

   if(id < wide)
      result[0] += A[id] * B[id];
 
}