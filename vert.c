uniform vec3 C; // -- camera in world coords
varying vec3 D; // -- normalized vector offset of camera

void main() {
    vec3 mv = vec3(gl_ModelViewMatrix * gl_Vertex);
    D = normalize(C - mv);
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; 
}
