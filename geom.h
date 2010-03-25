// excellent site for these things:
// http://iat.ubalt.edu/summers/math/platsol.htm

#define XYZ(f,T) f( \
    (C.x - (T) * D.x), \
    (C.y - (T) * D.y), \
    (C.z - (T) * D.z) \
)

// Intel GMA doesn't like the real sqrt
#define sqrt(f) length(vec3(f,0.0,0.0))

#define sphere(x,y,z) \
    x*x + y*y + z*z - 1.0

#define torus(x,y,z) \
    pow(1 - sqrt(x*x + y*y), 2) + z*z - 0.1

#define hyperboloid1(x,y,z) \
    x*x + y*y - z*z - 1.0

#define hyperboloid2(x,y,z) \
    -x*x - y*y + z*z - 1.0

#define screw(x,y,z) \
    0.5 * sin(x) + sqrt(y*y + z*z) - 1.5
