#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include "chess.h"

int main(void) {
	Board board = load_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w");
	//Board board = load_fen("4k3/pppp1ppp/8/8/4N3/8/8/4K3 b");
	//Board board = load_fen("3prp2/pppp1ppp/8/8/8/8/8/4K3 b");
	//Board board = load_fen("8/8/8/3p4/8/3P4/8/8 w");
	drawboard(board);
	int32_t value = valueboard(board);
	int32_t moves;
	Board *newboards = getmoves(board, &moves);
	printf("\n");
	for (int i = 0; i < moves; i++) {
		drawboard(newboards[i]);
		printf("\n");
	}

	printf("Value: %d\n", value);
	free(newboards);
	return 0;
}

