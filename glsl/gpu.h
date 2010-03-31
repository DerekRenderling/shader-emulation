// extra stuff for the gpu

float min(float x, float y) {
    return x < y ? x : y;
}

float min(float x, float y, float z, float w) {
    return min(min(x,y), min(z,w));
}
