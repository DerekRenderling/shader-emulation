#include "emulate.cpp"

/*
uniform vec3 C;
//uniform vec3 D;
varying vec3 D;
*/

#define X(T) (C.x + T * D.x)
#define Y(T) (C.y + T * D.y)
#define Z(T) (C.z + T * D.z)

#define sphere(C,D,T) \
    X(T) * X(T) + Y(T) * Y(T) + Z(T) * Z(T) - 1.0

#define torus(C,D,T) \
    pow(1 - sqrt(X*X + Y*Y), 2) + Z*Z - 0.1

#define surface(C,D,T) \
    sphere(C,D,T)

void main() {
    // P(t) = C + t * D, t >= 0
    float xn_a, xn_b = 0.0;
    
    // look from the camera outwards for a sign change
    xn_b = surface(C,D,0.0);
    for (float t = 1.0; t += 1.0; t < 10.0 ) {
        xn_a = surface(C,D,t);
        int a = xn_a > 0;
        int b = xn_b > 0;
        if ((a > 0 && b < 0) || (a < 0 && b > 0)) break;
        xn_b = xn_a;
    }
    
    // use the secant method on this interval
    const float epsilon = 0.01;
    float d;
    for (int i = 0; i < 4; i++) {
        float f_a = surface(C,D,xn_a);
        float f_b = surface(C,D,xn_b);
        d = (xn_a - xn_b) / (f_a - f_b) * f_a;
        if (abs(d) < epsilon) break; // found an acceptable root
        xn_b = xn_a;
        xn_a -= d;
    }
    
    if (abs(d) > epsilon) {
        gl_FragColor = vec4(0.0,1.0,0.0,1.0);
    }
    else {
        vec3 point = C + xn_a * D; // point of intersection
        //vec4 proj = gl_ProjectionMatrix * vec4(vec3(point),1.0);
        //gl_FragDepth = 0.1; // 0.5 + 0.5 * (proj.z / proj.w);
        gl_FragColor = vec4(1.0,0.0,0.0,1.0);
    }
}
