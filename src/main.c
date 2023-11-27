#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include "chess.h"

void test() {
	Board board = load_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w");
	int32_t moves = 0;
	Board newboards[1024];
	getmoves(board, &moves, newboards);
	for (int i = 0; i < moves; i++) {
		drawboard(newboards[i]);
		printf("\n");
	}

}

int main(void) {
	Board board = load_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w");
	//Board board = load_fen("4k3/pppp1ppp/8/8/4N3/8/8/4K3 b");
	//Board board = load_fen("k2prp2/pppp1ppp/8/8/8/8/8/4K3 b");
	//Board board = load_fen("8/8/8/3p4/2q5/3P4/8/8 b");
	drawboard(board);
	Board newboard = minimax(board, 5, true);
	//drawboard(newboard);
	printf("\n");
	bool max = true;
	for (int i = 0; i < 20; i++) {
		max = !max;
		newboard = minimax(newboard, 5, max);
		drawboard(newboard);
		printf("value: %d\n", valueboard(newboard));
		printf("\n");
	}
	//drawboard(newboard);
	printf("\n");
	printf("\n");
	printf("\n");
	int32_t moves = 0;
	Board newboards[1024];
	getmoves(newboard, &moves, newboards);
	for (int i = 0; i < moves; i++) {
		if (valueboard(newboards[i]) != 0) {
			drawboard(newboards[i]);
		printf("\n");
		}
	}
	printf("TESTING\n");
	//test();

	return 0;
}


