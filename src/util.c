#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "chess.h"

const char *letters = "prnbqk";

void placepiece(uint64_t *board, Piece piece, int square) {
	uint64_t squaremask = 1ul << square;
	board[piece] |= squaremask;
}

int calculatepos(char *pos) {
	int file = pos[0] - 'a';
	int rank = pos[1] - '1';
	return rank * 8 + file;
}


Board load_fen(char *fen) {
	Board board = {0};
	char pos[3] = "a1";
	while (*fen != ' ') {
		for (int i = PAWN; i < LAST; ++i) {
			if (*fen == letters[i]) {
				placepiece(board.black, i, calculatepos(pos));
			} else if (*fen == letters[i]- 'a'+ 'A') {
				placepiece(board.white, i, calculatepos(pos));
			}
		}

		for (int i = 0; i < 8; i++) {
			if (*fen == '1' + i) {
				pos[0] += i;
			}
		}

		if (*fen == '/') {
			pos[0] = 'a' -1 ;
			pos[1]++;
		}

		pos[0]++;
		fen++;
	}
	fen++;
	board.white_to_move = (*fen == 'w') ? true : false;

	return board;
}

char findchar(uint64_t *pieces, char shift, uint64_t square) {
	for (int i = PAWN; i < LAST; i++) {
		if (pieces[i] & square) {
			return letters[i] + shift;
		}
	}
	return '.';
}

void drawpieces(Board board) {
	for (int i = 0; i < 64; i++) {
		uint64_t square = 1ul << i;

		uint64_t *curpieces = board.black;
		char mult = 0;
		char piece = findchar(curpieces, mult, square);
		if (piece == '.') {
			curpieces = board.white;
			mult = 'A' - 'a';
			piece = findchar(curpieces, mult, square);
		}
		printf("%c", piece);

		if (i % 8 == 7) {
			printf("\n");
		}
	}

}

void drawboard(Board board) {
	drawpieces(board);
}

bool isvalidpos(char *pos) {
	if (pos[0] < 'a' || pos[0] > 'h') {
		return false;
	}
	if (pos[1] < '1' || pos[1] > '8') {
		return false;
	}
	return true;
}

bool isemptysquare(uint64_t *pieces, char *pos) {
	int square = calculatepos(pos);
	uint64_t squaremask = 1ul << square;
	uint64_t mask = pieces[PAWN] | pieces[ROOK] | pieces[KNIGHT] | pieces[BISHOP] | pieces[QUEEN] | pieces[KING];
	return !(mask & squaremask);
}

void movepice(uint64_t *pices, char *start, char *end) {
	int startpos = calculatepos(start);
	int endpos = calculatepos(end);
	uint64_t startmask = 1ul << startpos;
	uint64_t endmask = 1ul << endpos;
	*pices &= ~startmask;
	*pices |= endmask;
}


void apply_position(Board board, Board *moves, int *nummoves, char *newpos, char *pos,Piece king_field) {
	(*nummoves)++; \
	moves = realloc(moves, sizeof(Board) * (*nummoves)); \
	Board newboard; \
	memcpy(&newboard, &board, sizeof(Board)); \
	uint64_t *curpieces = newboard.white_to_move ? newboard.white : newboard.black;
	movepice(&curpieces[king_field], pos, newpos); \
	moves[*nummoves - 1] = newboard; \
}


int newkingpositions[8][3] = {
	{- 1, - 1},
	{- 1,   0},
	{- 1, + 1},
	{  0, - 1},
	{  0, + 1},
	{+ 1, - 1},
	{+ 1,   0},
	{+ 1, + 1},
};

int newknightpositions[8][3] = {
	{- 2, - 1},
	{- 2, + 1},
	{- 1, - 2},
	{- 1, + 2},
	{+ 1, - 2},
	{+ 1, + 2},
	{+ 2, - 1},
	{+ 2, + 1},
};

void bishopmoves(Board board, Board *moves, int *nummoves, char *pos, int direction) {
	for (int j = 1; j < 8; j++) {
		char newpos[2];
		if (direction == 0) {
			newpos[0] = pos[0] + j;
			newpos[1] = pos[1] + j;
		} else if (direction == 1) {
			newpos[0] = pos[0] - j;
			newpos[1] =	pos[1] - j;
		} else if (direction == 2) {
			newpos[0] =	pos[0] + j;
			newpos[1] = pos[1] - j;
		} else if (direction == 3) {
			newpos[0] =	pos[0] - j;
			newpos[1] = pos[1] + j;
		}
		uint64_t *curpieces = board.white_to_move ? board.white : board.black;
		uint64_t *notcurpieces = !board.white_to_move ? board.white : board.black;
		if (!isvalidpos(newpos)) {
			continue;
		}
		if (isemptysquare(curpieces, newpos) && isemptysquare(notcurpieces, newpos) ) { 
			apply_position(board, moves, nummoves, newpos, pos, BISHOP);
			continue;
		} 
		if (!isemptysquare(notcurpieces, newpos)) {
			apply_position(board, moves, nummoves, newpos, pos, BISHOP);
		}
		break;
	}
}

void rookmoves(Board board, Board *moves, int *nummoves, char *pos, int direction) {
	for (int j = 1; j < 8; j++) {
		char newpos[2];
		if (direction == 0) {
			newpos[0] = pos[0] + j;
			newpos[1] = pos[1];
		} else if (direction == 1) {
			newpos[0] = pos[0] - j;
			newpos[1] =	pos[1];
		} else if (direction == 2) {
			newpos[0] =	pos[0];
			newpos[1] = pos[1] - j;
		} else if (direction == 3) {
			newpos[0] =	pos[0];
			newpos[1] = pos[1] + j;
		}
		uint64_t *curpieces = board.white_to_move ? board.white : board.black;
		uint64_t *notcurpieces = !board.white_to_move ? board.white : board.black;
		if (!isvalidpos(newpos)) {
			continue;
		}
		if (isemptysquare(curpieces, newpos) && isemptysquare(notcurpieces, newpos) ) { 
			apply_position(board, moves, nummoves, newpos, pos, ROOK);
			continue;
		} 
		if (!isemptysquare(notcurpieces, newpos)) {
			apply_position(board, moves, nummoves, newpos, pos, ROOK);
		}
		break;
	}
}

Board *getmoves(Board board, int32_t *nummoves) {
	Board *moves = malloc(0);
	char pos[3] = "a1";
	uint64_t *curpieces = board.white_to_move ? board.white : board.black;
	for (int i = 0;i < 64; i++) {
		uint64_t square = 1ul << i;
		//KINGS
		if (curpieces[KING] & square) {

			for (int j = 0; j < 8; j++) {
				char newpos[3] = {pos[0] + newkingpositions[j][0], pos[1] + newkingpositions[j][1], '\0'};
				if (isvalidpos(newpos) && isemptysquare(curpieces, newpos)) {
					apply_position(board, moves, nummoves, newpos, pos, KING);
				}
			}
		//KNIGHTS
		} else if (curpieces[KNIGHT] & square) {
			for (int j = 0; j < 8; j++) {
				char newpos[3] = {pos[0] + newknightpositions[j][0], pos[1] + newknightpositions[j][1], '\0'};

				if (isvalidpos(newpos) && isemptysquare(curpieces, newpos)) { 
					apply_position(board, moves, nummoves, newpos, pos, KNIGHT);
				}
			}
		//ROOKS
		} else if (curpieces[ROOK] & square) {
			rookmoves(board, moves, nummoves, pos, 0);
			rookmoves(board, moves, nummoves, pos, 1);
			rookmoves(board, moves, nummoves, pos, 2);
			rookmoves(board, moves, nummoves, pos, 3);
		//BISHOPS
		} else if (curpieces[BISHOP] & square) {
			bishopmoves(board, moves, nummoves, pos, 0);
			bishopmoves(board, moves, nummoves, pos, 1);
			bishopmoves(board, moves, nummoves, pos, 2);
			bishopmoves(board, moves, nummoves, pos, 3);

		}

		pos[0]++;
		if (pos[0] > 'h') {
			pos[0] = 'a';
			pos[1]++;
		}
	}
	return moves;
}
