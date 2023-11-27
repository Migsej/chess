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
drawPieces fen = undefined

drawSquare :: Float -> Float -> Picture
drawSquare x y = translate (-( fromIntegral width/2)) (-( fromIntegral height/2)) 
               $ translate (x*squarewidth) (y*squarewidth) 
               $ polygon [(x, y) , (x + squarewidth, y), (x + squarewidth, y + squarewidth), (x, y + squarewidth)]



main :: IO ()
main = play window white 60 initialGame gameAsPicture (\_ x -> x) (\_ x -> x)

