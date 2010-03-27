#ifdef CPU
#include "emulate.hpp"
#else
varying vec3 C;
varying vec3 D;
#endif

#include "geom.h"
#define geometry sphere
#define surface(T) XYZ(geometry,T)

void main() {
    // this interval is pretty much good enough!
    float t_low = 0.0;
    float t_high = 1.0;
    
    float xn_a = t_high;
    float xn_b = t_low;
    
    // use the secant method on this interval
    //const float epsilon = 0.00027; // sphere
    //const float epsilon = 0.003; // hyperboloid1
    //const float epsilon = 0.008; // hyperboloid2
    const float epsilon = 0.0001; // torus
    float d;
    for (int i = 0; i < 15; i++) {
        float f_a = surface(xn_a);
        float f_b = surface(xn_b);
        d = (xn_a - xn_b) / (f_a - f_b) * f_a;
        if (abs(f_a-f_b) < epsilon) break; // found an acceptable root
        xn_b = xn_a;
        xn_a -= d;
    }
    
    if (abs(d) > epsilon) {
        gl_FragColor = vec4(0.5,0.5,0.5,1.0);
    }
    else {
        float T = t_high;
        vec3 P = C + T * D; // point of intersection
        float dt = 0.01;
        vec3 PX = P.x - (C - (geometry(P.x + dt, P.y, P.z) - T) * D);
        
        vec3 N = vec3(PX.x,0.0,0.0) * dt;
        gl_FragColor = vec4(N,1.0);
    }
}
