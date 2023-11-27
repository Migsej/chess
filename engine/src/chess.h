#include <stdint.h>
#include <stdbool.h>

typedef enum {
	PAWN,
	ROOK,
	KNIGHT,
	BISHOP,
	QUEEN,
	KING,
	LAST
} Piece;


typedef struct {
	uint64_t white[6];
	uint64_t black[6];
	bool white_to_move;
} Board;


void placepiece(uint64_t *board, Piece piece, int square);

void drawboard(Board board);

Board load_fen(char *fen);

void printboard(Board *board);

int32_t valueboard(Board board);

void getmoves(Board board, int32_t *nummoves, Board *moves);

Board minimax(Board board, int32_t depth, bool maximizingPlayer);
