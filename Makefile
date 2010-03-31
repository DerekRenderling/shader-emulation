all: clean frag ppm

clean:
	rm -f frag

stub:
	g++ -DCPU -O3 -Iglsl frag.stub.c -o frag

frag:
	g++ -DCPU -O3 -I. frag.c -o frag

ppm:
	#bash -c 'echo -e "(0,-3,0)\n(0,-1,0)\n(400,300)" | ./frag ppm > im.ppm'
	bash -c 'echo -e "(-0.2,-2.4,-1.7)\n(0,-0.1,-1)\n(400,300)" | ./frag ppm > im.ppm'
	eog im.ppm

shader:
	ghc --make sim/Main.hs sim/Shader.hs -o Main
