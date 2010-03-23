// emulate GLSL on the CPU since this is getting ridiculous

#include <iostream>
#include <unistd.h>

#include "glsl.h"

static vec4 gl_FragColor;
static vec3 C, D;

void _main();

int main(int argc, char *argv[]) {
    int width, height;
    std::cin >> width;
    std::cin >> height;
    
    // render shader output to pbm (http://en.wikipedia.org/wiki/Netpbm_format)
    std::cout << "P3" << std::endl;
    std::cout << width << " " << height << std::endl;
    std::cout << 255 << std::endl;
    
    // side view
    vec3 dir = vec3(0.0,-1.0,0.0);
    C = vec3(0,3,0);
    
    for (float y = -1.0; y < 1.0-0.0001; y += 2.0 / height) {
        for (float x = -1.0; x < 1.0-0.0001; x += 2.0 / width) {
            D = normalize(vec3(-x,1.0,-y) - dir);
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
