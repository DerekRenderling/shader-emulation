#ifdef CPU
#include "glsl/emulate.hpp"
#else
varying vec3 C;
varying vec3 D;
#include "glsl/gpu.h"
#endif
