#include "chess.h" 
#include <stdint.h>

int32_t countpieces(uint64_t bitboard, int value) {
	int result = 0;
	while (bitboard) {
		result += (bitboard & 1) * value;
		bitboard >>= 1;
	}
	return result;
}

int32_t countpicesboard(uint64_t pieces[6]) {
		int result = 0;
		result += countpieces(pieces[PAWN], 1);
		result += countpieces(pieces[KNIGHT], 3);
		result += countpieces(pieces[BISHOP], 3);
		result += countpieces(pieces[ROOK], 5);
		result += countpieces(pieces[QUEEN], 9);
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
