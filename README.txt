Implicit Surface Shader Emulation
---------------------------------
James Halliday

Render implicit solids without having to do any more maths beyond pasting the
formula from wikipedia. Also emulate GLSL on the CPU since
programming on the GPU directly is painful.

g++ -DCPU -O3 frag.c -o frag
echo 400 300 | ./frag > im.ppm; eog im.ppm 
