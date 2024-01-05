import pygame as pg
import os
import time

import communicate
pg.init()

piecenames = {
    "p" : "pawn",
    "r" : "rook",
    "n" : "knight",
    "b" : "bishop",
    "q" : "queen",
    "k" : "king"
}

def calculatepos(pos):
    file = pos[0] - ord('a')
    rank = pos[1] - 1
    return rank * 8 + file


def findchar(square, pieces):
    for i in range(6):
        if pieces[i] & square:
            return list(piecenames.keys())[i]
    return '.'


def load_board(board):
    result = []
    pos = [ord('a'), 8]
    for p in range(64):
        square = 1 << calculatepos(pos)

        c = findchar(square, board.white) 
        if c != '.':
            color = "white" 
            piece = piecenames[c]
        else :
            c = findchar(square, board.black) 
            if c != '.':
                color = "black" 
                piece = piecenames[c]
            else:
                if p % 8 == 7:
                    pos[0] = ord('a') - 1
                    pos[1] -= 1
                pos[0] += 1
                continue
        r, c = pos
        result.append(((chr(r), c), f"{color}-{piece}.png"))
        if p % 8 == 7:
            pos[0] = ord('a') - 1
            pos[1] -= 1
        pos[0] += 1
    return result

WIDTH, HEIGHT = 800,800

fps = 3

screen = pg.display.set_mode((WIDTH, HEIGHT))

clock = pg.time.Clock()
running = True

piecespath = "./pieces/"

piecesmap = {}
for path in os.listdir(piecespath):
    image = pg.image.load(piecespath + path)
    image = pg.transform.scale(image,(WIDTH//8,WIDTH//8))
    piecesmap[path] = image

chessboard = pg.transform.scale(pg.image.load("./Chessboard_green_squares.png"),(WIDTH,HEIGHT))


board = communicate.load_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w")

def drawboard(board, screen):
    for (x,y), path in load_board(board) :
        realpos = ((ord(x) - ord('a')) * WIDTH//8, ((8- y) * HEIGHT // 8))
        screen.blit(piecesmap[path], realpos)
    



font = pg.font.SysFont(None, 50)
BLUE = (0,0,255)

max = False

while running:
    for event in pg.event.get():
        if event.type == pg.QUIT:
            running = False
        elif event.type  == pg.KEYDOWN:
            if event.key == pg.K_ESCAPE:
                running = False


    screen.fill((255,255,255))
    screen.blit(chessboard, (0,0))    
    drawboard(board, screen)
    max = not max 
    t0 = time.time()
    board = communicate.minimax(board, 5, max)
    t1 = time.time()
    img = font.render(f"Time taken: {t1 - t0:.2f} seconds", True, BLUE)
    screen.blit(img, (0, 0))

    pg.display.flip()
    clock.tick(fps)

pg.quit()
