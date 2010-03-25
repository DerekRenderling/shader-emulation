// excellent site for these things:
// http://iat.ubalt.edu/summers/math/platsol.htm

#define X(T) (C.x - T * D.x)
#define Y(T) (C.y - T * D.y)
#define Z(T) (C.z - T * D.z)

#define sphere(T) \
    X(T)*X(T) + Y(T)*Y(T) + Z(T)*Z(T) - 1.0

#define torus(T) \
    pow(1 - sqrt(X(T)*X(T) + Y(T)*Y(T)), 2) + Z(T)*Z(T) - 0.1

#define hyperboloid1(T) \
    X(T)*X(T) + Y(T)*Y(T) - Z(T)*Z(T) - 1.0

#define hyperboloid2(T) \
    -X(T)*X(T) - Y(T)*Y(T) + Z(T)*Z(T) - 1.0

#define screw(T) \
    0.5 * sin(X(T)) + sqrt(Y(T)*Y(T) + Z(T)*Z(T)) - 1.5
