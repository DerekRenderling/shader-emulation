// emulate GLSL on the CPU since this is getting ridiculous

#include <iostream>
#include <unistd.h>
#include <cstdio>
#include <cstring>

#include "glsl.h"

static vec4 gl_FragColor;
static vec3 C, D;

void _main();

int main(int argc, char *argv[]) {
    // generate a ppm file sometimes
    bool ppm = 0;
    if (argc >= 2 && !strcmp(argv[1],"ppm")) ppm = 1;
    
    int width, height;
    std::cin >> width;
    std::cin >> height;
    
    if (ppm) {
        // render shader output to ppm (http://en.wikipedia.org/wiki/Netpbm_format)
        std::cout << "P3" << std::endl;
        std::cout << width << " " << height << std::endl;
        std::cout << 255 << std::endl;
    }
    
    //C = vec3(0,-3,0);
    C = vec3(0,-1,3);
    
    char *buf;
    if (!ppm) buf = new char[3 * width * height];
    
    int offset = 0;
    for (float y = -1.0; y < 1.0 - 0.0001; y += 2.0 / height) {
        for (float x = -1.0; x < 1.0 - 0.0001; x += 2.0 / width) {
            //D = normalize(vec3(x,-1,y));
            D = normalize(vec3(x,y,1));
            _main();
            
            float a = clamp(gl_FragColor.a, 0, 1);
            char r = clamp(gl_FragColor.r * 255, 0, 255) * a - 255 * (a - 1);
            char g = clamp(gl_FragColor.g * 255, 0, 255) * a - 255 * (a - 1);
            char b = clamp(gl_FragColor.b * 255, 0, 255) * a - 255 * (a - 1);
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
        fwrite(buf, 3, width*height, stdout);
        fflush(stdout);
    }
}

#define main _main
