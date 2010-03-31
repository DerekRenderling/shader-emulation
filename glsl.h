#ifdef CPU
#include "glsl/cpu.h"
#else
varying vec3 C;
varying vec3 D;
#include "glsl/gpu.h"
#endif
