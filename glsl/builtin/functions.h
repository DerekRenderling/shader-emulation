#ifndef GLSL_BUILTIN_FUNCTIONS_H
#define GLSL_BUILTIN_FUNCTIONS_H

#include <cmath>

#define app(f) \
    vec2 f(vec2 v) { \
        return vec2(f(v.x),f(v.y)); \
    } \
    vec3 f(vec3 v) { \
        return vec3(f(v.x),f(v.y),f(v.z)); \
    } \
    vec4 f(vec4 v) { \
        return vec4(f(v.x),f(v.y),f(v.z),f(v.w)); \
    }

#define app2(f) \
    vec2 f(vec2 v1, vec2 v2) { \
        return vec2(f(v1.x,v2.x),f(v1.y,v2.y)); \
    } \
    vec3 f(vec3 v1, vec3 v2) { \
        return vec3(f(v1.x,v2.x),f(v1.y,v2.y),f(v1.z,v2.z)); \
    } \
    vec4 f(vec4 v1, vec4 v2) { \
        return vec4(f(v1.x,v2.x),f(v1.y,v2.y),f(v1.z,v2.z),f(v1.w,v2.w)); \
    } \

// angle and trigonometry functions

float radians(float d) {
    return pi / 180.0 * d;
}
app(radians)

float degrees(float r) {
    return 180.0 / pi * r;
}
app(degrees)

app(sin)
app(cos)
app(tan)
app(asin)
app(acos)
app(atan)

float atan(float x, float y) {
    return atan2(x,y);
}
app2(atan)

#undef app2
#undef app3
#undef app4
#undef app

#endif
