#ifndef VEC_H
#include <iostream>
#include <sstream>
#include <cstdio>

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

std::ostream & operator<<(std::ostream & os, const vec3 & v) {
    return os << vec3(v).to_s();
}

std::ostream & operator<<(std::ostream & os, vec4 & v) {
    return os << v.to_s();
}

std::ostream & operator<<(std::ostream & os, const vec4 & v) {
    return os << vec4(v).to_s();
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
#endif
