main: main.o util.o chess.o
	gcc -g -o main main.o util.o chess.o

util.o: src/util.c
	gcc -g -c src/util.c -o util.o

chess.o: src/chess.c
	gcc -g -c src/chess.c -o chess.o

main.o: src/main.c
	gcc -g -c src/main.c -o main.o

clean:
	rm -f main
	rm -f *.o
