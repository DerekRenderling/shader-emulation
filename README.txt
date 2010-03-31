Implicit Surface Shader Emulation
---------------------------------
James Halliday

Render implicit solids without having to do any more maths beyond pasting the
formula from wikipedia and tweaking the root intervals. Also emulate GLSL on the
CPU since programming on the GPU directly is painful. The GPU coordinates seem
to be a bit off still since most development has gone into the CPU-emulated
version.

Those issues aside, here's some stuff to try:
    make ppm
        Generate a ppm file with ./frag and display it with eog.
    
    ./Main cpu
        Launch the shader in cpu emulation mode.
        * Standard wasd navigation with hjkl rotation that mostly works.
        * Toggle cpu/gpu with 'c'
        * Recompile frag.c on both the cpu and gpu with 'r'
        * Space bar prints the state
        * Escape exits the app

More features:
    * GPU code with real #directives, thanks to cpphs preprocessing

Some useful incidental features:
    * In cpu mode, you can use std::cerr for debugging
    * Real gcc error messages when compiling in cpu mode
