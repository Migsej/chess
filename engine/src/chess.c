#include "chess.h"
#include <stdlib.h>
#include <stdint.h>

int32_t countpieces(uint64_t bitboard, int value) {
  int32_t result;
  for(result = 0; bitboard; result += value) {
    bitboard &= bitboard -1;
  }
  return result;
}

const int values[] = {1,5,3,3,9};

int32_t countpicesboard(uint64_t pieces[]) {
  int result = 0;
  for (int i = PAWN; i < KING;++i) {
    result += countpieces(pieces[i], values[i]);
  }
  if (countpieces(pieces[KING], 1) == 0) {
    result = INT32_MIN;
  }
  return result;
}

int32_t valueboard(Board board) {
  int result = 0;
  result += countpicesboard(board.white);
  result -= countpicesboard(board.black);
  return result;
}

Board call_minimax(Board board, int32_t depth, bool maximizing_player) {
  return minimax(board, depth, maximizing_player, -INT32_MAX, INT32_MAX);
}

Board minimax(Board board, int32_t depth, bool maximizing_player, int32_t alpha, int32_t beta) {
  if (depth == 0) {
    return board;
  }
  int32_t value = maximizing_player ? -INT32_MAX : INT32_MAX;
  int32_t nummoves = 0;
  Board moves[1024];
  getmoves(board, &nummoves, moves);
  Board bestmove = {0};
  for (int i = 0; i < nummoves; ++i) {
    Board newboard = minimax(moves[i], depth - 1, !maximizing_player, alpha, beta);
    int32_t newvalue = valueboard(newboard);
    if (maximizing_player) {
      if (newvalue > value) {
	value = newvalue;
	bestmove = moves[i];
      }
      if (value > beta) break;
      if (alpha < value) {
	alpha = value;
      }
    } else {
      if (newvalue < value) {
	value = newvalue;
	bestmove = moves[i];
      }
      if (value < alpha) break;
      if (beta > value) {
	beta = value;
      }
    }
  }
  return bestmove;
}

