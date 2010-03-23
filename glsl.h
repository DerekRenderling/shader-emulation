#ifndef GLSL_H
#define GLSL_H

#include <cmath>
#include "vec.h"
#include "mat.h"

#define min fmin
#define max fmax
#define abs fabs
#define sin fsin
#define cos fcos
#define tan ftan

float clamp(float x, float a, float b) {
    return min(max(x,a),b);
}

#endif
