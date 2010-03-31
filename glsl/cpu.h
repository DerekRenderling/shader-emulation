// emulate GLSL on the CPU
#ifndef GLSL_CPU_H
#define GLSL_CPU_H

#include <cmath>
#include <iostream>
#include <unistd.h>
#include <cstdio>
#include <cstring>

#include "glsl/vec.h"
#include "glsl/mat.h"
#include "glsl/builtin.h"

static vec4 gl_FragColor;
static vec3 C, D;

void _main();

void render(char *buf, vec2 size, vec3 C, vec3 dir) {
    int offset = 0;
    
    // proportions independent of aspect ratios
    float as = size.x > size.y ? size.x / size.y : 1.0;
    float sa = size.x > size.y ? 1.0 : size.y / size.x;
    
    // opengl is backwards >_<
    for (int yi = size.y - 1; yi >= 0; yi--) {
        float y = sa * (2.0 * (yi / size.y) - 1.0);
        for (int xi = 0; xi < size.x; xi++) {
            float x = as * (2.0 * (xi / size.x) - 1.0);
            D = normalize(dir + vec3(x,-1,y));
            _main();
            
            float a = clamp(gl_FragColor.a, 0, 1);
            buf[offset+0] = // red
                clamp(gl_FragColor.r * 255, 0, 255) * a - 255 * (a - 1);
            buf[offset+1] = // green
                clamp(gl_FragColor.g * 255, 0, 255) * a - 255 * (a - 1);
            buf[offset+2] = // blue
                clamp(gl_FragColor.b * 255, 0, 255) * a - 255 * (a - 1);
            offset += 3;
        }
    }
}

int main(int argc, char *argv[]) {
    // generate a ppm file sometimes
    bool ppm = 0;
    if (argc >= 2 && !strcmp(argv[1],"ppm")) ppm = 1;
    vec2 prev_size = vec2(0.0,0.0);
    char *im = 0;
    
    while (1) {
        std::cin >> C;
        vec3 dir;
        std::cin >> dir;
        dir = normalize(dir);
        vec2 size;
        std::cin >> size;
        
        if (size != prev_size) {
            if (im) delete [] im;
            im = new char[int(3 * size.x * size.y)];
        }
        
        render(im, size, C, dir);
        if (ppm) {
            // render shader output to ppm (http://en.wikipedia.org/wiki/Netpbm_format)
            std::cout << "P6" << std::endl;
            std::cout << size.x << " " << size.y << std::endl;
            std::cout << 255 << std::endl;
            fflush(stdout);
        }
        fwrite(im, 3, size.x * size.y, stdout);
        fflush(stdout);
        if (ppm) break; // exit right away for ppm
    }
    return 0;
}

#define main _main

#endif
