uniform vec3 C; // -- camera in world coords
varying vec3 D; // -- normalized vector offset of camera

void main() {
    gl_Position = gl_Vertex;
    // since the proxy geometry is just a screen placed in front of the camera:
    D = normalize(vec3(gl_Vertex.x,-1.0,gl_Vertex.y));
}
