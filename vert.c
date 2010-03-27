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
        v.x,
        v.y,
        v.z
    ));
    //D = normalize(vec3(gl_Vertex.x,-1.0,gl_Vertex.y));
}
