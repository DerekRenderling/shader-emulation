#include "glsl.h"
#include "geom.h"
#define geometry torus
#define surface(T) XYZ(geometry,T)

// simulation can define iteration level at compile-time (which can be at runtime)
#ifndef ITERATIONS
#define ITERATIONS 10
#endif

// find some roots without an analytic solution
// or even an analytic derivative!
float secant_method(float xn_a, float xn_b, float epsilon) {
    float d;
    for (int i = 0; i < ITERATIONS; i++) {
        float f_a = surface(xn_a);
        float f_b = surface(xn_b);
        d = (xn_a - xn_b) / (f_a - f_b) * f_a;
        if (abs(f_a-f_b) < epsilon) break; // found an acceptable root
        xn_b = xn_a;
        xn_a -= d;
    }
    if (abs(d) > epsilon) {
        return -1.0; // probably no solution
    }
    else {
        return xn_b;
    }
}

// approximate the surface normal of the surface
// the same way that the secant method above uses
vec3 surfaceN(vec3 P, float xn_a, float dt) {
    float xn_b = xn_a + dt;
    float f_a = surface(xn_a);
    float f_b = surface(xn_b);
    float dx = (f_a - f_b) / (xn_a - xn_b);
    vec3 S = C * dx + D;
    return cross(P, S);
}

// minimum positive value, -1.0 if all values are negative

float pmin(float a, float b) {
    float m = min(a,b);
    if (m >= 0.0) return m;
    float n = max(a,b);
    if (n >= 0.0) return n;
    return -1.0;
}

float pmin(float a, float b, float c) {
    return pmin(pmin(a,b),c);
}

float pmin(float a, float b, float c, float d) {
    return pmin(pmin(a,b),pmin(c,d));
}

void main() {
    const vec4 bg = vec4(0.5,0.5,0.5,1.0);
    
    // use the secant method on this interval
    //const float epsilon = 0.00027; // sphere
    //const float epsilon = 0.003; // hyperboloid1
    //const float epsilon = 0.008; // hyperboloid2
    const float epsilon = 1e-16; // torus
    const float d = length(C);
    
    float t = pmin(
        secant_method(d - 1.1, d - 1.0, epsilon),
        secant_method(d - 1.0, d - 0.9, epsilon),
        secant_method(d + 1.1, d + 1.0, epsilon),
        secant_method(d + 1.0, d + 0.9, epsilon)
    );
    if (t < 0.0) {
        gl_FragColor = bg;
    }
    else {
        vec3 P = C + t * D; // point of intersection
        vec3 N = surfaceN(P, t, 0.01);
        gl_FragColor = vec4(clamp(N,0.0,1.0),1.0);
    }
}
