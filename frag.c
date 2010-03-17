#ifdef GPU
uniform vec3 C;
varying vec3 D;
#else
#include "emulate.cpp"
#endif

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
    // find an interval for the secant method
    
    //std::cerr << "--------------------" << std::endl;
    // find nearest sign change with increasing multiples of two
    // probably should use some smaller multiple
    float t_low = 0.0, t_high = 1.0;
    int sign = surface(C,D,t_low);
    for (; t_high <= 64.0; t_high *= 2.0) {
        int a = surface(C,D,t_high) > 0;
        int b = sign > 0;
        if ((a && !b) || (!a && b)) break;
        t_low = t_high;
    }
    //std::cerr << t_low << "," << t_high << std::endl;
    
    // then binary search on that interval to a fixed resolution
    while (t_high - t_low > 0.1) {
        float mid = 0.5 * (t_high + t_low);
        float mid_a = 0.5 * (t_high + t_low) + 0.01;
        float mid_b = 0.5 * (t_high + t_low) - 0.01;
        
        // follow lower side
        if (surface(C,D,mid_a) > surface(C,D,mid_b)) {
            t_high = mid;
        }
        else {
            t_low = mid;
        }
    }
    //std::cerr << t_low << "," << t_high << std::endl;
    
    float xn_a = surface(C,D,t_high);
    float xn_b = surface(C,D,t_low);
    
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
    //std::cerr << xn_a << "," << xn_b << std::endl;
    
    if (d != d) { // NaN
        gl_FragColor = vec4(0.0,0.0,0.0,0.0);
    }
    else if (abs(d) > epsilon) {
        gl_FragColor = vec4(0.0,1.0,0.0,1.0);
    }
    else {
        vec3 point = C + xn_a * D; // point of intersection
        gl_FragColor = vec4(1.0,0.0,0.0,1.0);
    }
}
