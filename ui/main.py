import pygame as pg
import os

piecenames = {
    "p" : "pawn",
    "r" : "rook",
    "n" : "knight",
    "b" : "bishop",
    "q" : "queen",
    "k" : "king"
}

def load_fen(fen):
    result = []
    pos = [ord('a') - 1, 8]
    for c in fen:
        if c.isnumeric():
            for i in range(8):
                if c == chr(ord('1') + i):
                    pos[0] += i + 1
            continue

        if c == '/':
            pos[1] -= 1
            pos[0] = ord('a') - 1 
            continue
        if c == ' ':
            break
        # we know that it is a piece now
        if c.isupper():
            color = "white" 
            piece = piecenames[chr(ord(c) - ord('A') + ord('a'))]
        else :
            color = "black" 
            piece = piecenames[c]
        r, c = pos
        result.append(((chr(r + 1), c), f"{color}-{piece}.png"))
        pos[0] += 1
    return result

WIDTH, HEIGHT = 400,400

fps = 60

screen = pg.display.set_mode((WIDTH, HEIGHT))

clock = pg.time.Clock()
running = True

piecespath = "./pieces/"

piecesmap = {}
for path in os.listdir(piecespath):
    image = pg.image.load(piecespath + path)
    image = pg.transform.scale(image,(WIDTH//8,WIDTH//8))
    piecesmap[path] = image

board = pg.transform.scale(pg.image.load("./Chessboard_green_squares.png"),(400,400))


startfen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"
startfen = "4k3/pppp1ppp/8/8/4N3/8/8/4K3 b"

def drawfen(fen: str, screen):
    for (x,y), path in load_fen(fen) :
        realpos = ((ord(x) - ord('a')) * WIDTH//8, ((8- y) * HEIGHT // 8))
        print(realpos)
        screen.blit(piecesmap[path], realpos)
    



while running:
    for event in pg.event.get():
        if event.type == pg.QUIT:
            running = False
        elif event.type  == pg.KEYDOWN:
            if event.key == pg.K_ESCAPE:
                running = False


    screen.fill((255,255,255))
    screen.blit(board, (0,0))    
    drawfen(startfen, screen)

    pg.display.flip()
    clock.tick(fps)

pg.quit()
