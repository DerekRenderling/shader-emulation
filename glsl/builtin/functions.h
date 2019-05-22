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
    vec2 f(vec2 a, vec2 b) { \
        return vec2(f(a.x,b.x),f(a.y,b.y)); \
    } \
    vec3 f(vec3 a, vec3 b) { \
        return vec3( \
            f(a.x,b.x), \
            f(a.y,b.y), \
            f(a.z,b.z) \
        ); \
    } \
    vec4 f(vec4 a, vec4 b) { \
        return vec4( \
            f(a.x,b.x), \
            f(a.y,b.y), \
            f(a.z,b.z), \
            f(a.w,b.w) \
        ); \
    } \

// component-wise operations on two vectors and one float
#define app_v2f(f) \
    vec2 f(vec2 a, vec2 b, float x) { \
        return vec2(f(a.x,b.x,x),f(a.y,b.y,x)); \
    } \
    vec3 f(vec3 a, vec3 b, float x) { \
        return vec3( \
            f(a.x,b.x,x), \
            f(a.y,b.y,x), \
            f(a.z,b.z,x) \
        ); \
    } \
    vec4 f(vec4 a, vec4 b, float x) { \
        return vec4( \
            f(a.x,b.x,x), \
            f(a.y,b.y,x), \
            f(a.z,b.z,x), \
            f(a.w,b.w,x) \
        ); \
    } \

// component-wise operations on three vectors
#define app_v3(f) \
    vec2 f(vec2 a, vec2 b, vec2 c) { \
        return vec2( \
            f(a.x,b.x,c.x), \
            f(a.y,b.y,c.y) \
        ); \
    } \
    vec3 f(vec3 a, vec3 b, vec3 c) { \
        return vec3( \
            f(a.x,b.x,c.x), \
            f(a.y,b.y,c.y), \
            f(a.z,b.z,c.z) \
        ); \
    } \
    vec4 f(vec4 a, vec4 b, vec4 c) { \
        return vec4( \
            f(a.x,b.x,c.x), \
            f(a.y,b.y,c.y), \
            f(a.z,b.z,c.z), \
            f(a.w,b.w,c.w) \
        ); \
    } \

#define def_v1(f,a,expr) \
    float f(float a) { expr; } \
    vec2 f(vec2 a) { expr; } \
    vec3 f(vec3 a) { expr; } \
    vec4 f(vec4 a) { expr; } \

#define def_v2(f,a,b,expr) \
    f(float a, float b) { expr; } \
    f(vec2 a, vec2 b) { expr; } \
    f(vec3 a, vec3 b) { expr; } \
    f(vec4 a, vec4 b) { expr; } \

#define defT_v1(f,a,expr) \
    float f(float a, float b) { expr; } \
    vec2 f(vec2 a) { expr; } \
    vec3 f(vec3 a) { expr; } \
    vec4 f(vec4 a) { expr; } \

#define defT_v2(f,a,b,expr) \
    float f(float a, float b) { expr; } \
    vec2 f(vec2 a, vec2 b) { expr; } \
    vec3 f(vec3 a, vec3 b) { expr; } \
    vec4 f(vec4 a, vec4 b) { expr; } \

#define defT_v2f(f,a,b,c,expr) \
    float f(float a, float b, float c) { expr; } \
    vec2 f(vec2 a, vec2 b, float c) { expr; } \
    vec3 f(vec3 a, vec3 b, float c) { expr; } \
    vec4 f(vec4 a, vec4 b, float c) { expr; } \

#define defT_v3(f,a,b,c,expr) \
    float f(float a, float b, float c) { expr; } \
    vec2 f(vec2 a, vec2 b, vec2 c) { expr; } \
    vec3 f(vec3 a, vec3 b, vec2 c) { expr; } \
    vec4 f(vec4 a, vec4 b, vec2 c) { expr; } \

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
/***float abs(float x) {
    return fabs(x);
}***/
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

// geometric functions
// none of these are component-wise
float length(float x) {
    return abs(x);
}

float length(vec2 v) {
    return sqrt(v.x*v.x + v.y*v.y);
}

float length(vec3 v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
}

float length(vec4 v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z + v.w*v.w);
}

def_v2(float distance,a,b,
    return length(a-b)
);

float dot(float a, float b) {
    return a * b;
}

float dot(vec2 a, vec2 b) {
    return a.x * b.x + a.y * b.y;
}

float dot(vec3 a, vec3 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

float dot(vec4 a, vec4 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
}

vec3 cross(vec3 a, vec3 b) {
    return vec3(
        a.y * b.z - a.z * b.y,
        a.z * b.x - a.x * b.z,
        a.x * b.y - a.y * b.x
    );
}

defT_v1(normalize,x, return x / length(x));

// ignoring ftransform for now

defT_v3(faceforward, N, I, Nref,
    return dot(Nref,I) < 0.0 ? N : -N
);

defT_v2(reflect, I, N,
    return I - 2 * dot(N,I) * N
);

defT_v2f(refract, I, N, eta,
    float k = 1.0 - eta * eta * (1.0 - dot(N,I) * dot(N,I));
    return k < 0.0
        ? I * 0.0
        : eta * I - (eta * dot(N,I) + sqrt(k)) * N
);

#undef app_v
#undef app_vf
#undef app_vf2
#undef app_v2
#undef app_v3

#endif
