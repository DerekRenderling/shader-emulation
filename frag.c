uniform vec3 C;
//uniform vec3 D;
varying vec3 D;

#define X (C.x + T * D.x)
#define Y (C.y + T * D.y)
#define Z (C.z + T * D.z)

#define sphere(C,D,T) \
    X * X + Y * Y + Z * Z - 1.0

#define torus(C,D,T) \
    pow(1 - sqrt(X*X + Y*Y), 2) + Z*Z - 0.1

#define surface(C,D,T) \
    sphere(C,D,T)

void main() {
    // P(t) = C + t * D, t >= 0
    float xn = 1.1;
    float xn_prev = 0.1;
    
    // root finding using the secant method
    const float epsilon = 0.01;
    float d;
    for (int i = 0; i < 4; i++) {
        float f_xn = surface(C,D,xn);
        float f_xn_prev = surface(C,D,xn_prev);
        d = (xn - xn_prev) / (f_xn - f_xn_prev) * f_xn;
        if (abs(d) < epsilon) break; // found an acceptable zero
        xn_prev = xn;
        xn -= d;
    }
    
    if (abs(d) > epsilon) {
        gl_FragColor = vec4(0.0,1.0,0.0,1.0);
    }
    else {
        vec3 point = C + xn * D; // point of intersection
        vec4 proj = gl_ProjectionMatrix * vec4(vec3(point),1.0);
        gl_FragDepth = 0.1; // 0.5 + 0.5 * (proj.z / proj.w);
        gl_FragColor = vec4(1.0,0.0,0.0,1.0);
    }
}
