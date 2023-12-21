main: main.o util.o chess.o
	gcc -Wall -g -o main main.o util.o chess.o

util.o: src/util.c
	gcc -Wall -g -c src/util.c -o util.o

chess.o: src/chess.c
	gcc -Wall -g -c src/chess.c -o chess.o

main.o: src/main.c
	gcc -Wall -g -c src/main.c -o main.o

libmain: libutil.o libchess.o libmain.o
	gcc -Wall -g -shared -fPIC -o libmain.so main.o util.o chess.o

libutil.o: src/util.c
	gcc -Wall -g -c -fPIC src/util.c -o util.o

libchess.o: src/chess.c
	gcc -Wall -g -c -fPIC src/chess.c -o chess.o

libmain.o: src/main.c
	gcc -Wall -g -c -fPIC src/main.c -o main.o



clean:
	rm -f main
	rm -f *.o
