#include <iostream>
#include <sstream>
#include <unistd.h>
#include <cstdio>

struct vec4;

struct vec3 {
    union {
        struct { float x, y, z; };
        struct { float r, g, b; };
    };
    
    vec3(float x_, float y_, float z_) {
        x = x_; y = y_; z = z_;
    }
    
    vec3(float f) {
        x = f; y = 0.0; z = 0.0;
    }
    
    vec3(const vec3 & v) {
        x = v.x; y = v.y; z = v.z;
    }
    
    std::string to_s() {
        std::stringstream s;
        s << "(" << x << "," << y << "," << z << ")";
        return s.str();
    }
    
    vec3(const vec4 & v);
};

struct vec4 {
    union {
        struct { float x, y, z, w; };
        struct { float r, g, b, a; };
    };
    
    vec4(float x_, float y_, float z_, float w_) {
        x = x_; y = y_; z = z_; w = w_;
    }
    
    vec4(float f) {
        x = f; y = 0.0; z = 0.0; w = 0.0;
    }
    
    vec4(const vec4 & v) {
        x = v.x; y = v.y; z = v.z; w = v.w;
    }
    
    vec4(const vec3 & v, float f) {
        x = v.x; y = v.y; z = v.z; w = f;
    }
    
    std::string to_s() {
        std::stringstream s;
        s << "(" << x << "," << y << "," << z << "," << w << ")";
        return s.str();
    }
};

vec3::vec3(const vec4 & v) {
    x = v.x; y = v.y; z = v.z;
}

std::ostream & operator<<(std::ostream & os, vec3 & v) {
    return os << v.to_s();
}

std::ostream & operator<<(std::ostream & os, vec4 & v) {
    return os << v.to_s();
}

std::istream & operator>>(std::istream & is, vec3 & v) {
    std::string tuple;
    is >> tuple;
    float x, y, z;
    scanf("(%f,%f,%f)", tuple.c_str(), &x, &y, &z);
    v.x = x; v.y = y; v.z = z;
    return is;
}

void _main(vec3 C, vec3 D);

int main(int argc, char *argv[]) {
    int width, height;
    std::cin >> width >> height;
    char *buf = new char[width * height * 4];
    int i = 0;
    
    vec3 C = vec3(0.0,-5.0,0.0);
    
    for (float y = -1.0; y < 1.0; y += 2.0 / height) {
        for (float x = -1.0; x < 1.0; x += 2.0 / width) {
            vec3 D = vec3(x, y, 0.0) - C;
            vec4 c;
            _main(C,D,c);
            buf[i] = (unsigned char *) {
                min(max(c.r * 256, 0), 255),
                min(max(c.g * 256, 0), 255),
                min(max(c.b * 256, 0), 255),
                min(max(c.a * 256, 0), 255)
            };
            i += 4;
        }
        fwrite(buf, sizeof(char) * 4, width, stdout);
    }
}

#define main _main(vec3 C, vec3 D, vec4 & gl_FragColor) { //
#define uniform //
#define varying //

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
        vec4 proj = gl_ProjectionMatrix * vec4(vec3(point),1.0);
        gl_FragDepth = 0.1; // 0.5 + 0.5 * (proj.z / proj.w);
        gl_FragColor = vec4(1.0,0.0,0.0,1.0);
    }
}
