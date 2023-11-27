main: main.o util.o chess.o
	gcc -Wall -g -o main main.o util.o chess.o

util.o: src/util.c
	gcc -Wall -g -c src/util.c -o util.o

chess.o: src/chess.c
	gcc -Wall -g -c src/chess.c -o chess.o

main.o: src/main.c
	gcc -Wall -g -c src/main.c -o main.o

clean:
	rm -f main
	rm -f *.o
