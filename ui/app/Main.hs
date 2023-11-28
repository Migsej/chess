module Main where

import Graphics.Gloss

width :: Int
width = 800 

squarewidth :: Float
squarewidth = fromIntegral width / 8

height :: Int
height = 800 


window :: Display
window = InWindow "chess" (width, height) (0, 0)

data Turn = White | Black

data Game = Game {
     fen :: String
    ,turn :: Turn
}

initfen :: String
initfen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

initialGame :: IO Game
initialGame = return Game {fen = initfen, turn = White}

gameAsPicture :: IO Game -> Picture
gameAsPicture game = pictures [drawBoard, drawPieces (fen <$> game)] 

drawBoard :: Picture
drawBoard = pictures [drawSquare x y | x <- [0..7]
                                     , y <- [0..7]
                                     , even (round x + round y)]

drawPieces :: IO String -> Picture
drawPieces fen = undefined -- (getcharpos ('a', 8)) <$> fen $ undefined

type Pos = (Char, Int)
data Piece = Pawn | Knight | Bishop | Rook | Queen | King

type PieceColor = (Piece, Bool)

drawPiece :: PieceColor -> Pos -> Picture
drawPiece (piece, color) pos = undefined



getcharpos :: Pos -> String -> [(PieceColor, Pos)]
getcharpos pos  (x:xs) 
                | x == '/' = getcharpos ('a', snd pos - 1) xs 
                | x `elem` ['1'..'8'] = getcharpos (fst pos, snd pos + read [x]) xs 
                | otherwise = (getpiece x, pos) : getcharpos (succ (fst pos), snd pos) xs 

getcharpos _ [] = []

getpiece :: Char -> PieceColor
getpiece 'p' = (Pawn, False)
getpiece 'P' = (Pawn, True)
getpiece 'n' = (Knight, False)
getpiece 'N' = (Knight, True)
getpiece 'b' = (Bishop, False)
getpiece 'B' = (Bishop, True)
getpiece 'r' = (Rook, False)
getpiece 'R' = (Rook, True)
getpiece 'q' = (Queen, False)
getpiece 'Q' = (Queen, True)
getpiece 'k' = (King, False)
getpiece 'K' = (King, True)
getpiece _ = error "invalid piece"

drawSquare :: Float -> Float -> Picture
drawSquare x y = translate (-( fromIntegral width/2)) (-( fromIntegral height/2)) 
               $ translate (x*squarewidth) (y*squarewidth) 
               $ polygon [(x, y) , (x + squarewidth, y), (x + squarewidth, y + squarewidth), (x, y + squarewidth)]



main :: IO ()
main = play window white 60 initialGame gameAsPicture (\_ x -> x) (\_ x -> x)

