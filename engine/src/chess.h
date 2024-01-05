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

typedef enum {
  WHITE_TO_MOVE = 2,
  WHITE_KINGSIDE,
  WHITE_QUEENSIDE,
  BLACK_KINGSIDE,
  BLACK_QUEENSIDE,
} Flags;

typedef struct {
  uint64_t white[6];
  uint64_t black[6];
  unsigned char flags;
} Board;


void placepiece(uint64_t *board, Piece piece, int square);

void drawboard(Board board);

Board load_fen(char *fen);

void printboard(Board *board);

int32_t valueboard(Board board);

void getmoves(Board board, int32_t *nummoves, Board *moves);

Board minimax(Board board, int32_t depth, bool maximizing_player, int32_t alpha, int32_t beta);

Board call_minimax(Board board, int32_t depth, bool maximizing_player);
