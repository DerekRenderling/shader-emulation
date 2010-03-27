// emulate GLSL on the CPU since this is getting ridiculous

#include <iostream>
#include <unistd.h>
#include <cstdio>
#include <cstring>
#include <cmath>

#include "glsl.h"

static vec4 gl_FragColor;
static vec3 C, D;

void _main();

int main(int argc, char *argv[]) {
    // generate a ppm file sometimes
    bool ppm = 0;
    if (argc >= 2 && !strcmp(argv[1],"ppm")) ppm = 1;
    
    vec2 size;
    std::cin >> size;
    std::cin >> C;
    vec3 dir;
    std::cin >> dir;
    dir = normalize(dir);
    
    float theta = atan2(dir.x,dir.z);
    float rho = atan2(dir.y,dir.z);
    
    if (ppm) {
        // render shader output to ppm (http://en.wikipedia.org/wiki/Netpbm_format)
        std::cout << "P3" << std::endl;
        std::cout << size.x << " " << size.y << std::endl;
        std::cout << 255 << std::endl;
    }
    
    char *buf;
    if (!ppm) buf = new char[int(3 * size.x * size.y)];
    int offset = 0;
    
    // proportions independent of aspect ratios
    float as = size.x > size.y ? size.x / size.y : 1.0;
    float sa = size.x > size.y ? 1.0 : size.y / size.x;
    
    for (int yi = 0; yi < size.y; yi++) {
        float y = sa * (2.0 * (yi / size.y) - 1.0);
        for (int xi = 0; xi < size.x; xi++) {
            float x = as * (2.0 * (xi / size.x) - 1.0);
            //D = normalize(vec3(x,-1,y));
            //D = normalize(vec3(x,y,1));
            D = normalize(dir + vec3(x,-1,y));
            
            //std::cerr << normalize(vec3(x,-1,y)) << "  versus  " << D << std::endl;
            
            _main();
            
            float a = clamp(gl_FragColor.a, 0, 1);
            unsigned int r =
                clamp(gl_FragColor.r * 255, 0, 255) * a - 255 * (a - 1);
            unsigned int g =
                clamp(gl_FragColor.g * 255, 0, 255) * a - 255 * (a - 1);
            unsigned int b =
                clamp(gl_FragColor.b * 255, 0, 255) * a - 255 * (a - 1);
            if (ppm) {
                std::cout << r << " " << g << " " << b << " ";
            }
            else {
                buf[offset+0] = r;
                buf[offset+1] = g;
                buf[offset+2] = b;
                offset += 3;
            }
        }
    }
    
    if (!ppm) {
        fwrite(buf, 3, size.x * size.y, stdout);
        fflush(stdout);
    }
}

#define main _main
