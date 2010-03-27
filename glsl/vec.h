#ifndef VEC_H
#define VEC_H
#include <iostream>
#include <sstream>
#include <cstdio>

struct vec2;
struct vec3;
struct vec4;

struct vec2 {
    union {
        struct { float x, y; };
    };
    
    vec2() {}
    
    vec2(float x_, float y_) {
        x = x_; y = y_;
    }
    
    vec2(const vec2 & v) {
        x = v.x; y = v.y;
    }
    
    float & operator[](int i) {
        switch (i) {
            case 0 : return x;
            case 1 : return y;
        }
        std::cerr << "Out of bounds index " << i << " on vec2" <<std::endl;
        return (*this)[i % 2];
    }
    
    std::string to_s() {
        std::stringstream s;
        s << "(" << x << "," << y << ")";
        return s.str();
    }
    
    vec2(const vec3 &);
    vec2(const vec4 &);
};

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
    
    vec3(const vec3 & v) {
        x = v.x; y = v.y; z = v.z;
    }
    
    float & operator[](int i) {
        switch (i) {
            case 0 : return x;
            case 1 : return y;
            case 2 : return z;
        }
        std::cerr << "Out of bounds index " << i << " on vec3" <<std::endl;
        return (*this)[i % 3];
    }
    
    std::string to_s() {
        std::stringstream s;
        s << "(" << x << "," << y << "," << z << ")";
        return s.str();
    }
    
    vec3(const vec4 &);
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
    
    vec4(const vec4 & v) {
        x = v.x; y = v.y; z = v.z; w = v.w;
    }
    
    vec4(const vec3 & v, float f) {
        x = v.x; y = v.y; z = v.z; w = f;
    }
    
    float & operator[](int i) {
        switch (i) {
            case 0 : return x;
            case 1 : return y;
            case 2 : return z;
            case 3 : return w;
        }
        std::cerr << "Out of bounds index " << i << " on vec4" <<std::endl;
        return (*this)[i % 4];
    }
    
    std::string to_s() {
        std::stringstream s;
        s << "(" << x << "," << y << "," << z << "," << w << ")";
        return s.str();
    }
};

#define op_vec2(OPN,OP) \
    vec2 OPN(const vec2 v1, const vec2 v2) { \
        return vec2(v1.x OP v2.x, v1.y OP v2.y); \
    } \
    vec2 OPN(const vec2 v, float f) { \
        return vec2(v.x OP f, v.y OP f); \
    } \
    vec2 OPN(float f, const vec2 v) { \
        return vec2(v.x OP f, v.y OP f); \
    }

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

#define op_vec(OPN,OP) \
    op_vec2(OPN,OP) \
    op_vec3(OPN,OP) \
    op_vec4(OPN,OP)

op_vec(operator+,+);
op_vec(operator-,-);
op_vec(operator*,*);
op_vec(operator/,/);

vec2::vec2(const vec3 & v) {
    x = v.x; y = v.y;
}

vec2::vec2(const vec4 & v) {
    x = v.x; y = v.y;
}

vec3::vec3(const vec4 & v) {
    x = v.x; y = v.y; z = v.z;
}

std::ostream & operator<<(std::ostream & os, vec2 & v) {
    return os << v.to_s();
}

std::ostream & operator<<(std::ostream & os, vec3 & v) {
    return os << v.to_s();
}

std::ostream & operator<<(std::ostream & os, vec4 & v) {
    return os << v.to_s();
}

std::ostream & operator<<(std::ostream & os, const vec2 & v) {
    return os << vec2(v).to_s();
}

std::ostream & operator<<(std::ostream & os, const vec3 & v) {
    return os << vec3(v).to_s();
}

std::ostream & operator<<(std::ostream & os, const vec4 & v) {
    return os << vec4(v).to_s();
}

std::istream & operator>>(std::istream & is, vec2 & v) {
    std::string tok;
    is >> tok;
    float x, y, z;
    sscanf(tok.c_str(), "(%f,%f)", &x, &y);
    v.x = x; v.y = y;
    return is;
}

std::istream & operator>>(std::istream & is, vec3 & v) {
    std::string tok;
    is >> tok;
    float x, y, z;
    sscanf(tok.c_str(), "(%f,%f,%f)", &x, &y, &z);
    v.x = x; v.y = y; v.z = z;
    return is;
}

std::istream & operator>>(std::istream & is, vec4 & v) {
    std::string tok;
    is >> tok;
    float x, y, z, w;
    scanf(tok.c_str(), "(%f,%f,%f,%f)", &x, &y, &z, &w);
    v.x = x; v.y = y; v.z = z; v.w = w;
    return is;
}

float length(vec2 v) {
    return sqrt(v.x*v.x + v.y*v.y);
}

float length(vec3 v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
}

float length(vec4 v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z + v.w*v.w);
}

float dot(vec2 a, vec2 b) {
    return a.x * b.x + a.y * b.y;
}

float dot(vec3 a, vec3 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

float dot(vec4 a, vec4 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
}

vec2 normalize(vec2 v) {
    return v / length(v);
}

vec3 normalize(vec3 v) {
    return v / length(v);
}

vec4 normalize(vec4 v) {
    return v / length(v);
}

vec3 cross(vec3 v1, vec3 v2) {
    vec3(
        v1.y * v2.z - v1.z * v2.y,
        v1.z * v2.x - v1.x * v2.z,
        v1.x * v2.y - v1.y * v2.x
    );
}

#endif
