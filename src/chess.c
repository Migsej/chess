#include "chess.h" 
#include <stdint.h>

int32_t countpieces(uint64_t bitboard, int value) {
	int32_t result;
	for(result = 0; bitboard; result++) {
		bitboard &= bitboard -1;
	}
	return result;
}

const int values[] = {1,3,3,5,9};

int32_t countpicesboard(uint64_t pieces[6]) {
		int result = 0;
		for (int i = PAWN; i < KING;++i) {
			result += countpieces(pieces[i], values[i]);
		}
		if (!countpieces(pieces[KING], 1)) {
			result = INT32_MAX;
		}
		return result;
}

int32_t valueboard(Board board) {
	int result = 0;
	result += countpicesboard(board.white);
	result -= countpicesboard(board.black);
	return result;
}
