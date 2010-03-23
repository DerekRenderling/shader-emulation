#ifndef GLSL_H
#define GLSL_H

#include <cmath>
#include "vec.h"
#include "mat.h"

#define min fmin
#define max fmax
#define abs fabs

float clamp(float x, float a, float b) {
    return min(max(x,a),b);
}

#endif
