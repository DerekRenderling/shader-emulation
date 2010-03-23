#ifdef CPU
#include "emulate.cpp"
#else
uniform vec3 C;
varying vec3 D;
#endif

#define X(T) (C.x - T * D.x)
#define Y(T) (C.y - T * D.y)
#define Z(T) (C.z - T * D.z)

#define sphere(T) \
    X(T) * X(T) + Y(T) * Y(T) + Z(T) * Z(T) - 1.0

#define torus(T) \
    pow(1 - sqrt(X(T)*X(T) + Y(T)*Y(T)), 2) + Z(T)*Z(T) - 0.1

#define hyperboloid1(T) X(T)*X(T) + Y(T)*Y(T) - Z(T)*Z(T) - 1.0
#define hyperboloid2(T) -X(T)*X(T) - Y(T)*Y(T) + Z(T)*Z(T) - 1.0

#define surface(T) hyperboloid2(T)

void main() {
    // P(t) = C + t * D, t >= 0
    // find an interval for the secant method
    
    float t_low = 0.0;
    float t_high = 1.0;
    
    float xn_a = t_high;
    float xn_b = t_low;
    
    // use the secant method on this interval
    const float epsilon = 0.0001;
    float d;
    for (int i = 0; i < 10; i++) {
        float f_a = surface(xn_a);
        float f_b = surface(xn_b);
        d = (xn_a - xn_b) / (f_a - f_b) * f_a;
        if (abs(f_a-f_b) < epsilon) break; // found an acceptable root
        xn_b = xn_a;
        xn_a -= d;
    }
    
    if (d != d) { // NaN
        gl_FragColor = vec4(1.0,1.0,0.0,0.0);
    }
    else if (abs(d) > epsilon) {
        gl_FragColor = vec4(0.0,1.0,0.0,1.0);
    }
    else {
        vec3 point = C + t_high * D; // point of intersection
        gl_FragColor = vec4(point.x,0.0,point.z,1.0);
    }
}
