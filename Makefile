all: clean frag ppm

clean:
	rm -f frag

stub:
	g++ -DCPU -O3 -Iglsl frag.stub.c -o frag

frag:
	g++ -DCPU -O3 -I. frag.c -o frag

ppm:
	bash -c 'echo -e "(0.0,-2.4,-1.4)\n(0.0,-0.3,-0.9)\n(400,300)" | ./frag ppm > im.ppm'
	eog im.ppm

shader:
	ghc --make sim/Main.hs sim/Shader.hs -o Main
