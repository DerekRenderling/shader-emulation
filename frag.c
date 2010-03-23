#ifdef CPU
#include "emulate.cpp"
#else
uniform vec3 C;
varying vec3 D;
#endif

#define X(T) (C.x - T * D.x)
#define Y(T) (C.y - T * D.y)
#define Z(T) (C.z - T * D.z)

#define sphere(C,D,T) \
    X(T) * X(T) + Y(T) * Y(T) + Z(T) * Z(T) - 1.0

#define torus(C,D,T) \
    pow(1 - sqrt(X(T)*X(T) + Y(T)*Y(T)), 2) + Z(T)*Z(T) - 0.1

#define surface(C,D,T) \
    torus(C,D,T)

void main() {
    // P(t) = C + t * D, t >= 0
    // find an interval for the secant method
    
    #ifdef DEBUG
    std::cerr << "--------------------" << std::endl;
    #endif
    
    // find nearest sign change with increasing multiples of two
    // probably should use some smaller multiple
    float t_low = 0.0, t_high = 1.0;
    float sign = surface(C,D,t_low);
    for (; t_high <= 64.0; t_high *= 2.0) {
        int a = surface(C,D,t_high) > 0;
        int b = sign > 0;
        
        #ifdef DEBUG
        std::cerr << "search t="<<t_low << "," << t_high << std::endl;
        #endif
        
        if ((a && !b) || (!a && b)) break;
        t_low = t_high;
    }
    
    // then binary search on that interval to a fixed resolution
    while (t_high - t_low > 0.01) {
        float mid = 0.5 * (t_high + t_low);
        int a = surface(C,D,t_high)>0;
        int b = surface(C,D,mid)>0;
        // int c=surface(C,D,t_low)>0;
        
        if (a == b) { // transition is between mid and lo
            t_high = mid;
        }
        else { // transition is between hi and mid
            t_low = mid;
        }
        
        #ifdef DEBUG
        std::cerr << "bisection t="<<t_low << "," << t_high << std::endl;
        #endif
    }
    
    float xn_a = t_high; // surface(C,D,t_high);
    float xn_b = t_low; // surface(C,D,t_low);
    
    // use the secant method on this interval
    const float epsilon = 0.01;
    float d;
    for (int i = 0; i < 10; i++) {
        float f_a = surface(C,D,xn_a);
        float f_b = surface(C,D,xn_b);
        d = (xn_a - xn_b) / (f_a - f_b) * f_a;
        if (abs(f_a-f_b) < epsilon) break; // found an acceptable root
        xn_b = xn_a;
        xn_a -= d;
        
        #ifdef DEBUG
        std::cerr << "Secant t="<<xn_a << "," << xn_b << std::endl;
        #endif
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
    
 //   gl_FragColor = vec4(xn_a,xn_b,0.0,1.0);
}
