#define sphere(T) \
    X(T) * X(T) + Y(T) * Y(T) + Z(T) * Z(T) - 1.0

#define torus(T) \
    pow(1 - sqrt(X(T)*X(T) + Y(T)*Y(T)), 2) + Z(T)*Z(T) - 0.1

#define hyperboloid1(T) \
    X(T)*X(T) + Y(T)*Y(T) - Z(T)*Z(T) - 1.0

#define hyperboloid2(T) \
    -X(T)*X(T) - Y(T)*Y(T) + Z(T)*Z(T) - 1.0

#define wavy(T) \
    0.5 * sin(X(T)) + sqrt(Y(T)*Y(T) + Z(T)*Z(T)) - 1.5
