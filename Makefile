all: clean frag demo

clean:
	rm -f frag

frag:
	g++ -DCPU -O3 -Iglsl frag.c -o frag

demo:
	@echo 400 300 | ./frag > im.ppm; eog im.ppm 
