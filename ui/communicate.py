from ctypes import *

lib = CDLL("../engine/libmain.so")

'''
typedef struct {
	uint64_t white[6];
	uint64_t black[6];
	bool white_to_move;
} Board;
'''
class Board(Structure):
    _fields_ = [("white", c_uint64 * 6),
                ("black", c_uint64 * 6),
                ("white_to_move", c_bool)]
'''
typedef enum {
	PAWN,
	ROOK,
	KNIGHT,
	BISHOP,
	QUEEN,
	KING,
	LAST
} Piece;
'''
PAWN = 0
ROOK = 1
KNIGHT = 2
BISHOP = 3
QUEEN = 4
KING = 5



#Board load_fen(char *fen);
lib.load_fen.argtypes = [c_char_p]
lib.load_fen.restype = Board


#Board minimax(Board board, int32_t depth, bool maximizingPlayer);
lib.call_minimax.argtypes = [Board, c_int32, c_bool]
lib.call_minimax.restype = Board

def load_fen(fen):
    return lib.load_fen(fen.encode())

def minimax(board, depth, max):
    return lib.call_minimax(board, depth, max)


