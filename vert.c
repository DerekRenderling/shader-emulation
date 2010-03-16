uniform vec3 C; // -- camera in world coords
//uniform vec3 D; // -- normalized vector offset of camera
varying vec3 D; // -- normalized vector offset of camera

void main() {
    vec3 mv = vec3(gl_ModelViewMatrix * gl_Vertex);
    D = normalize(mv - C);
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; 
}
