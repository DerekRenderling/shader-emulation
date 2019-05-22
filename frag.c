#include "glsl.h"

//#define geometry torus

// Try this one too! At runtime even! Just hit 'r'
#define geometry sphere

#include "geom.h"

void main() {
    const vec4 bg = vec4(0.5,0.5,0.5,1.0);
    
    // use the secant method on this interval
    const float d = length(C);
    
    // These are pretty well fine-tuned for the torus, but they work well enough
    // for the sphere too. If I had more time I could probably get rid of the
    // discontinuities in the transition.
    float t = pmin(
        secant_method(d - 1.2, d - 1.19, 1e-13),
        secant_method(d + 1.1, d + 1.0, 1e-13)
    );
    if (t < 0.0) {
        gl_FragColor = bg;
    }
    else {
        vec3 P = C + t * D; // point of intersection
        vec3 N = surfaceN(P, t, 0.01);
        vec3 L = normalize(vec3(3.0, 4.0, 5.0));
        float c = clamp(dot(N,L),0.0,1.0);
        gl_FragColor = vec4(clamp(N,0.0,1.0),1.0);
        ///gl_FragColor = vec4(c,c,c,1.0);
    }
}
