#ifndef GLSL_BUILTIN_FUNCTIONS_H
#define GLSL_BUILTIN_FUNCTIONS_H

#include <cmath>

// component-wise operations on one vector
#define app_v(f) \
    vec2 f(vec2 v) { \
        return vec2(f(v.x),f(v.y)); \
    } \
    vec3 f(vec3 v) { \
        return vec3(f(v.x),f(v.y),f(v.z)); \
    } \
    vec4 f(vec4 v) { \
        return vec4(f(v.x),f(v.y),f(v.z),f(v.w)); \
    }

// component-wise operations on one vector and one float
#define app_vf(f) \
    vec2 f(vec2 v, float x) { \
        return vec2(f(v.x,x),f(v.y,x)); \
    } \
    vec3 f(vec3 v, float x) { \
        return vec3(f(v.x,x),f(v.y,x),f(v.z,x)); \
    } \
    vec4 f(vec4 v, float x) { \
        return vec4(f(v.x,x),f(v.y,x),f(v.z,x),f(v.w,x)); \
    }

// component-wise operations on one float and one vector
#define app_fv(f) \
    vec2 f(float x, vec2 v) { \
        return vec2(f(v.x,x),f(v.y,x)); \
    } \
    vec3 f(float x, vec3 v) { \
        return vec3(f(v.x,x),f(v.y,x),f(v.z,x)); \
    } \
    vec4 f(float x, vec4 v) { \
        return vec4(f(v.x,x),f(v.y,x),f(v.z,x),f(v.w,x)); \
    }

// component-wise operations on one vector and two floats
#define app_vf2(f) \
    vec2 f(vec2 v, float x, float y) { \
        return vec2(f(v.x,x,y),f(v.y,x,y)); \
    } \
    vec3 f(vec3 v, float x, float y) { \
        return vec3(f(v.x,x,y),f(v.y,x,y),f(v.z,x,y)); \
    } \
    vec4 f(vec4 v, float x, float y) { \
        return vec4(f(v.x,x,y),f(v.y,x,y),f(v.z,x,y),f(v.w,x,y)); \
    }

// component-wise operations on two floats and one vector
#define app_f2v(f) \
    vec2 f(float x, float y, vec2 v) { \
        return vec2(f(v.x,x,y),f(v.y,x,y)); \
    } \
    vec3 f(float x, float y, vec3 v) { \
        return vec3(f(v.x,x,y),f(v.y,x,y),f(v.z,x,y)); \
    } \
    vec4 f(float x, float y, vec4 v) { \
        return vec4(f(v.x,x,y),f(v.y,x,y),f(v.z,x,y),f(v.w,x,y)); \
    }

// component-wise operations on two vectors
#define app_v2(f) \
    vec2 f(vec2 v1, vec2 v2) { \
        return vec2(f(v1.x,v2.x),f(v1.y,v2.y)); \
    } \
    vec3 f(vec3 v1, vec3 v2) { \
        return vec3( \
            f(v1.x,v2.x), \
            f(v1.y,v2.y), \
            f(v1.z,v2.z) \
        ); \
    } \
    vec4 f(vec4 v1, vec4 v2) { \
        return vec4( \
            f(v1.x,v2.x), \
            f(v1.y,v2.y), \
            f(v1.z,v2.z), \
            f(v1.w,v2.w) \
        ); \
    } \

// component-wise operations on two vectors and one float
#define app_v2f(f) \
    vec2 f(vec2 v1, vec2 v2, float x) { \
        return vec2(f(v1.x,v2.x,x),f(v1.y,v2.y,x)); \
    } \
    vec3 f(vec3 v1, vec3 v2, float x) { \
        return vec3( \
            f(v1.x,v2.x,x), \
            f(v1.y,v2.y,x), \
            f(v1.z,v2.z,x) \
        ); \
    } \
    vec4 f(vec4 v1, vec4 v2, float x) { \
        return vec4( \
            f(v1.x,v2.x,x), \
            f(v1.y,v2.y,x), \
            f(v1.z,v2.z,x), \
            f(v1.w,v2.w,x) \
        ); \
    } \

// component-wise operations on three vectors
#define app_v3(f) \
    vec2 f(vec2 v1, vec2 v2, vec2 v3) { \
        return vec2( \
            f(v1.x,v2.x,v3.x), \
            f(v1.y,v2.y,v3.y) \
        ); \
    } \
    vec3 f(vec3 v1, vec3 v2, vec3 v3) { \
        return vec3( \
            f(v1.x,v2.x,v3.x), \
            f(v1.y,v2.y,v3.y), \
            f(v1.z,v2.z,v3.z) \
        ); \
    } \
    vec4 f(vec4 v1, vec4 v2, vec4 v3) { \
        return vec4( \
            f(v1.x,v2.x,v3.x), \
            f(v1.y,v2.y,v3.y), \
            f(v1.z,v2.z,v3.z), \
            f(v1.w,v2.w,v3.w) \
        ); \
    } \

// angle and trigonometry functions

float radians(float d) {
    return pi / 180.0 * d;
}
app_v(radians)

float degrees(float r) {
    return 180.0 / pi * r;
}
app_v(degrees)

app_v(sin)
app_v(cos)
app_v(tan)
app_v(asin)
app_v(acos)
app_v(atan)

float atan(float x, float y) {
    return atan2(x,y);
}
app_v2(atan)

// exponential functions
app_v2(pow)
app_v(exp)
app_v(log)
app_v(exp2)
app_v(log2)
app_v(sqrt)
float inversesqrt(float x) {
    return 1.0 / sqrt(x);
}
app_v(inversesqrt)

// common functions
float abs(float x) {
    return fabs(x);
}
app_v(abs)

float sign(float x) {
    return x > 0.0 ? 1.0 : 0.0;
}
app_v(sign)

app_v(floor)
app_v(ceil)

float fract(float x) {
    return x - floor(x);
}
app_v(fract)

float mod(float x, float y) {
    return x - y * floor(x / y);
}
app_v2(mod)
app_vf(mod)

float min(float x, float y) {
    return fmin(x,y);
}
app_v2(min)
app_vf(min)

float max(float x, float y) {
    return fmax(x,y);
}
app_v2(max)
app_vf(max)

float clamp(float x, float min_v, float max_v) {
    return min(max(x,min_v),max_v);
}
app_v3(clamp)
app_vf2(clamp)

float mix(float x, float y, float a) {
    return x * (1.0 - a) + y * a;
}
app_v3(mix)
app_v2f(mix)

float step(float edge, float x) {
    return x < edge ? 0.0 : 1.0;
}
app_v2(step)
app_fv(step)

float smoothstep(float edge0, float edge1, float x) {
    float t = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    return t * t * (3 - 2 * t);
}
app_f2v(smoothstep)
app_v3(smoothstep)

#undef app_v
#undef app_vf
#undef app_vf2
#undef app_v2
#undef app_v3

#endif
