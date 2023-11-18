#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "chess.h"

void placepiece(uint64_t *board, Piece piece, int square) {
	uint64_t squaremask = 1ul << square;
	switch (piece) {
		case PAWN:
			board[PAWN] |= squaremask;
			break;
		case ROOK:
			board[ROOK] |= squaremask;
			break;
		case KNIGHT:
			board[KNIGHT] |= squaremask;
			break;
		case BISHOP:
			board[BISHOP] |= squaremask;
			break;
		case QUEEN:
			board[QUEEN] |= squaremask;
			break;
		case KING:
			board[KING] |= squaremask;
			break;
	}
}

int calculatepos(char *pos) {
	int file = pos[0] - 'a';
	int rank = pos[1] - '1';
	return rank * 8 + file;
}

#define PARSEPIECE(char, piece) \
	case char: \
		placepiece(board.black, piece, calculatepos(pos)); \
		break; \
	case char - 'a' + 'A': \
		placepiece(board.white, piece, calculatepos(pos)); \
		break;

#define PARSENUM(char, num) \
	case char: \
		pos[0] += num - 1; \
		break;

Board load_fen(char *fen) {
	Board board = {0};
	char pos[3] = "a1";
	while (*fen != ' ') {
		switch (*fen) {
			PARSEPIECE('p', PAWN);
			PARSEPIECE('r', ROOK);
			PARSEPIECE('n', KNIGHT);
			PARSEPIECE('b', BISHOP);
			PARSEPIECE('q', QUEEN);
			PARSEPIECE('k', KING);
			case '/':
				pos[0] = 'a' -1 ;
				pos[1]++;
				break;
			PARSENUM('1', 1);
			PARSENUM('2', 2);
			PARSENUM('3', 3);
			PARSENUM('4', 4);
			PARSENUM('5', 5);
			PARSENUM('6', 6);
			PARSENUM('7', 7);
			PARSENUM('8', 8);
		}
		pos[0]++;
		fen++;
	}
	fen++;
	board.white_to_move = (*fen == 'w') ? true : false;

	return board;
}
char findchar(uint64_t *pieces, char mult, uint64_t square) {
	if (pieces[PAWN] & square) {
		return 'p' + mult;
	} else if (pieces[ROOK] & square) {
		return 'r' + mult;
	} else if (pieces[KNIGHT] & square) {
		return 'n' + mult;
	} else if (pieces[BISHOP] & square) {
		return 'b' + mult;
	} else if (pieces[QUEEN] & square) {
		return 'q' + mult;
	} else if (pieces[KING] & square) {
		return 'k' + mult;
	} else {
		return '.';
	}
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

bool isvalidpos(Board board, char *pos) {
	if (pos[0] < 'a' || pos[0] > 'h') {
		return false;
	}
	if (pos[1] < '1' || pos[1] > '8') {
		return false;
	}

	int square = calculatepos(pos);
	uint64_t squaremask = 1ul << square;
	uint64_t *pieces = board.white_to_move ? board.white : board.black;
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
	{- 1,- 1},
	{- 1,  0,},
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
				if (isvalidpos(board, newpos)) {
					apply_position(board, moves, nummoves, newpos, pos, KING);
				}
			}
		//KNIGHTS
		} else if (curpieces[KNIGHT] & square) {
			for (int j = 0; j < 8; j++) {
				char newpos[3] = {pos[0] + newknightpositions[j][0], pos[1] + newknightpositions[j][1], '\0'};

				if (isvalidpos(board, newpos)) { 
					apply_position(board, moves, nummoves, newpos, pos, KNIGHT);
				}
			}
		//ROOKS
		} else if (curpieces[ROOK] & square) {
			for (int j = pos[0] - 8; j < pos[0] + 8; j++) {
				char newpos[3] = { j, pos[1], '\0'};
				if (!isvalidpos(board, newpos)) { 
					if (j < pos[0]) {
						j = pos[0];
						continue;
					} else {
						break;
					}
				} 
				apply_position(board, moves, nummoves, newpos, pos, ROOK);
			}
			for (int j = pos[1] - 8; j < pos[1] + 8; j++) {
				char newpos[3] = {pos[0], j, '\0'};
				if (!isvalidpos(board, newpos)) { 
					if (j < pos[1]) {
						j = pos[1];
						continue;
					} else {
						break;
					}
				} 
				apply_position(board, moves, nummoves, newpos, pos, ROOK);
			}
		}

		pos[0]++;
		if (pos[0] > 'h') {
			pos[0] = 'a';
			pos[1]++;
		}
	}
	return moves;
}
