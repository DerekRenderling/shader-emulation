uniform vec3 pos; // camera center
uniform vec3 dir; // camera direction
varying vec3 C; // ray start position
varying vec3 D; // ray direction

void main() {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    C = pos;
    D = normalize(vec3(gl_Vertex) - dir);
}
