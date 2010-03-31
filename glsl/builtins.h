#ifndef GLSL_EXTRA_H
#define GLSL_EXTRA_H

#include "glsl/vec.h"

float min(float x, float y, float z) {
    return min(min(x,y),z);
}

float min(float x, float y, float z, float w) {
    return min(min(x,y,z),w);
}

float max(float x, float y) {
    return x > y ? x : y;
}

float max(float x, float y, float z) {
    return max(max(x,y),z);
}

float max(float x, float y, float z, float w) {
    return max(max(x,y,z),w);
}

float clamp(float x, float a, float b) {
    return min(max(x,a),b);
}

#endif
vec3 min

#endif
