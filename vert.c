uniform vec3 pos; // camera position
uniform vec2 dir; // camera direction as (theta, rho) angles
varying vec3 C; // ray start position
varying vec3 D; // ray direction

void main() {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    C = pos;
    // since the proxy geometry is just a screen placed in front of the camera:
    vec3 v = vec3(gl_Vertex.x, -1.0, gl_Vertex.y);
    D = normalize(vec3(
        v.x * cos(dir.x) + v.z * sin(dir.x),
        v.y * cos(dir.y) + v.x * sin(dir.x),
        v.y * sin(dir.y) + v.z * cos(dir.y)
    ));
    //D = normalize(vec3(gl_Vertex.x,-1.0,gl_Vertex.y));
}
