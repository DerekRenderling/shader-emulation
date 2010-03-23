// emulate GLSL on the CPU since this is getting ridiculous

#include <iostream>
#include <unistd.h>
#include <cmath>

#include "vec.h"

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

float length(vec3 v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
}

float length(vec4 v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z + v.w*v.w);
}

vec3 normalize(vec3 v) {
    return v / length(v);
}

vec4 normalize(vec4 v) {
    return v / length(v);
}

int main(int argc, char *argv[]) {
    int width, height;
    std::cin >> width;
    std::cin >> height;
    
    // render shader output to pbm (http://en.wikipedia.org/wiki/Netpbm_format)
    std::cout << "P3" << std::endl;
    std::cout << width << " " << height << std::endl;
    std::cout << 255 << std::endl;
    
    C = vec3(0.0,0.0,-10.0);
    
    for (float y = -1.0; y < 1.0-0.0001; y += 2.0 / height) {
        for (float x = -1.0; x < 1.0-0.0001; x += 2.0 / width) {
            D = C - vec3(x, 0.0, y);
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
