uniform vec3 C; // -- camera in world coords
varying vec3 D; // -- normalized vector offset of camera

void main() {
    vec4 mv = gl_ModelViewMatrix * gl_Vertex;
    gl_Position = gl_ProjectionMatrix * mv;
    D = normalize(vec3(gl_Vertex.x,-1,gl_Vertex.y));
}
