all: clean frag demo

clean:
	rm -f frag

stub:
	g++ -DCPU -O3 -Iglsl frag.stub.c -o frag

frag:
	g++ -DCPU -O3 -Iglsl frag.c -o frag -fopenmp

demo:
	@echo 400 300 | ./frag ppm > im.ppm; eog im.ppm 

shader:
	ghc --make sim/Main.hs sim/Shader.hs -o Main
