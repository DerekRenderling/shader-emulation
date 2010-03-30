#ifdef CPU
#include "emulate.hpp"
#else
varying vec3 C;
varying vec3 D;
#endif

#include "geom.h"
#define geometry torus
#define surface(T) XYZ(geometry,T)

float secant(float xn_a, float xn_b, float epsilon) {
    float d;
    for (int i = 0; i < 10; i++) {
        float f_a = surface(xn_a);
        float f_b = surface(xn_b);
        d = (xn_a - xn_b) / (f_a - f_b) * f_a;
        if (abs(f_a-f_b) < epsilon) break; // found an acceptable root
        xn_b = xn_a;
        xn_a -= d;
    }
    if (abs(d) > epsilon) {
        return -1.0;
    }
    else {
        return xn_b;
    }
}

void main() {
    // use the secant method on this interval
    //const float epsilon = 0.00027; // sphere
    //const float epsilon = 0.003; // hyperboloid1
    //const float epsilon = 0.008; // hyperboloid2
    const float epsilon = 1e-12; // torus
    const float d = length(C);
    
    float t1 = secant(d - 1.1, d - 1.0, epsilon);
    float t2 = secant(d - 1.0, d - 0.9, epsilon);
    if (t2 < 0.0) t2 = t1;
    float t3 = secant(d + 1.1, d + 1.0, epsilon);
    if (t3 < 0.0) t3 = t2;
    float t4 = secant(d + 1.0, d + 0.9, epsilon);
    if (t4 < 0.0) t4 = t3;
    
    if (t4 < 0.0) {
        gl_FragColor = vec4(0.5,0.5,0.5,1.0);
    }
    else {
        //float T = t1;
        //float T = min(t1,t2);
        //float T = min(t1,t2,t3);
        float T = min(t1,t2,t3,t4);
        vec3 P = C + T * D; // point of intersection
        float dt = 0.01;
        vec3 PX = P.x - (C - (geometry(P.x + dt, P.y, P.z) - T) * D);
        
        vec3 N = vec3(PX.x,0.0,0.0) * dt;
        gl_FragColor = vec4(N,1.0);
    }
}
