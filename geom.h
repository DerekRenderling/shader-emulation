// excellent site for these things:
// http://iat.ubalt.edu/summers/math/platsol.htm

#define XYZ(f,T) f( \
    (C.x - (T) * D.x), \
    (C.y - (T) * D.y), \
    (C.z - (T) * D.z) \
)

// Intel GMA doesn't like the real sqrt
#define sqrt(f) length(vec3(f,0.0,0.0))

#define sphere(x,y,z) \
    x*x + y*y + z*z - 1.0

#define torus(x,y,z) \
    pow(1 - sqrt(x*x + y*y), 2) + z*z - 0.1

#define hyperboloid1(x,y,z) \
    x*x + y*y - z*z - 1.0

#define hyperboloid2(x,y,z) \
    -x*x - y*y + z*z - 1.0

#define screw(x,y,z) \
    0.5 * sin(x) + sqrt(y*y + z*z) - 1.5

// simulation can define iteration level at compile-time (which can be at runtime)
#ifndef ITERATIONS
#define ITERATIONS 1000
#endif

#define surface(T) XYZ(geometry,T)

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
