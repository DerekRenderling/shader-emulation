// emulate GLSL on the CPU since this is getting ridiculous
#include <iostream>
#include <sstream>
#include <unistd.h>
#include <cstdio>
#include <cmath>

struct vec4;

struct vec3 {
    union {
        // use xyz or rgb attributes
        struct { float x, y, z; };
        struct { float r, g, b; };
    };
    
    vec3() {}
    
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
        // use xyzw or rgba attributes
        struct { float x, y, z, w; };
        struct { float r, g, b, a; };
    };
    
    vec4() {}
    
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

#define op_vec3(OPN,OP) \
    vec3 OPN(const vec3 v1, const vec3 v2) { \
        return vec3(v1.x OP v2.x, v1.y OP v2.y, v1.z OP v2.z); \
    } \
    vec3 OPN(const vec3 v, float f) { \
        return vec3(v.x OP f, v.y OP f, v.z OP f); \
    } \
    vec3 OPN(float f, const vec3 v) { \
        return vec3(v.x OP f, v.y OP f, v.z OP f); \
    }

#define op_vec4(OPN,OP) \
    vec4 OPN(const vec4 v1, const vec4 v2) { \
        return vec4(v1.x OP v2.x, v1.y OP v2.y, v1.z OP v2.z, v1.w OP v2.w); \
    } \
    vec4 OPN(const vec4 v, float f) { \
        return vec4(v.x OP f, v.y OP f, v.z OP f, v.w OP f); \
    } \
    vec4 OPN(float f, const vec4 v) { \
        return vec4(v.x OP f, v.y OP f, v.z OP f, v.w OP f); \
    }

op_vec3(operator+,+);
op_vec3(operator-,-);
op_vec3(operator*,*);
op_vec3(operator/,/);

op_vec4(operator+,+);
op_vec4(operator-,-);
op_vec4(operator*,*);
op_vec4(operator/,/);

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

std::istream & operator>>(std::istream & is, vec4 & v) {
    std::string tuple;
    is >> tuple;
    float x, y, z, w;
    scanf("(%f,%f,%f,%f)", tuple.c_str(), &x, &y, &z, &w);
    v.x = x; v.y = y; v.z = z; v.w = w;
    return is;
}

#define min fmin
#define max fmax
#define abs fabs
#define sin fsin
#define cos fcos
#define tan ftan

static vec4 gl_FragColor;

static vec3 C, D;
void _main();

float clamp(float x, float a, float b) {
    return min(max(x,a),b);
}

int main(int argc, char *argv[]) {
    int width, height;
    std::cin >> width;
    std::cin >> height;
    
    // render shader output to PPM
    std::cout << "P3" << std::endl;
    std::cout << width << " " << height << std::endl;
    std::cout << 255 << std::endl;
    
    vec3 C = vec3(0.0,-5.0,0.0);
    
    for (float y = -1.0; y < 1.0; y += 2.0 / height) {
        for (float x = -1.0; x < 1.0; x += 2.0 / width) {
            D = vec3(x, 0.0, y) + C;
            _main();
            
            float a = clamp(gl_FragColor.a, 0, 1);
            int r = clamp(gl_FragColor.r * 255, 0, 255) * a - 255 * (a - 1);
            int g = clamp(gl_FragColor.g * 255, 0, 255) * a - 255 * (a - 1);
            int b = clamp(gl_FragColor.b * 255, 0, 255) * a - 255 * (a - 1);
            
            std::cout << r << " " << g << " " << b << " ";
        }
        std::cout << std::endl;
    }
}

#define main _main
