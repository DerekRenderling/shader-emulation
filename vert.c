uniform vec3 C; // -- camera in world coords
uniform vec3 D0; // -- normalized vector offset of camera
varying vec3 D; // -- normalized vector offset of camera

void main() {
    D = normalize(vec3(gl_Vertex) + D0);
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; 
}
