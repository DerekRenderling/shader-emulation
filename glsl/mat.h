#ifndef MAT_H
#define MAT_H
#include "vec.h"

struct mat4 {
    vec4 cols[4];
    
    mat4() {
        cols[0] = vec4(1.0,0.0,0.0,0.0);
        cols[1] = vec4(0.0,1.0,0.0,0.0);
        cols[2] = vec4(0.0,0.0,1.0,0.0);
        cols[3] = vec4(0.0,0.0,0.0,1.0);
    }
    
    // create matrix by columns
    mat4(vec4 a, vec4 b, vec4 c, vec4 d) {
        cols[0] = a;
        cols[1] = b;
        cols[2] = c;
        cols[3] = d;
    }
    
    // return column i
    vec4 & operator[](int i) {
        return cols[i];
    }
    
    vec4 column(int i) {
        return cols[i];
    }
    
    vec4 row(int i) {
        return vec4(cols[0][i], cols[1][i], cols[2][i], cols[3][i]);
    }
    
    mat4 operator+(mat4 & m) {
        mat4 z;
        for (int i = 0; i < 4; i++)
            z[i] = cols[i] + m[i];
        return z;
    }
    
    mat4 operator*(mat4 & m) {
        mat4 z;
        for (int c = 0; c < 4; c++) {
            vec4 col;
            for (int r = 0; r < 4; r++) {
                col[r] = dot(m[c], vec4(row(r)));
            }
            z[c] = col;
        }
        return z;
    }
};

#endif
