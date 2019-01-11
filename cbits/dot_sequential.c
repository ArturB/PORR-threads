#include <stdio.h>
#include <stdlib.h>

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


int main() {
    double* m1 = malloc(40 * sizeof(double));
    double* m2 = malloc(40*6000 * sizeof(double));
    for(int i = 0; i < 40; ++i) {
        m1[i] = i;
    }
    for(int i = 0; i < 40*6000; ++i) {
        m2[i] = i;
    } 
    const int times = 1800; 
    printf("Starting calulating matrix product %i times...\n", times); fflush(stdout);
    for(int i = 0; i < times; ++i) {
        double* m3 = matrixMult(m1, m2, 1, 40, 40, 6000);
        double** dm3 = matrixDeserializeAsVector(m3, 1, 6000);
        matrixFreeMem(dm3, 1, 6000); 
        free(m3);
    }
    //double** dm3 = matrixDeserializeAsVector(matrixMult(m2, m2, 3, 3, 3, 3), 3, 3);
    //matrixPrintf(dm3, 3, 3); matrixFreeMem(dm3, 3, 3);
    printf("All done!\n"); 
}
