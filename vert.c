uniform vec3 C; // -- camera in world coords
uniform vec3 D; // -- normalized vector offset of camera

void main() {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; 
}
