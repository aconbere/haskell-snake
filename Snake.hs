module Snake where

import Prelude hiding (Right, Left)
import Piece
import Angle
import Shape
import Color

snakePiece position angle =
    SnakePiece (Square 1) Red position angle

nextSnake snake angle =
    (drop 1 snake) ++ [snakePiece nextPos angle]
    where nextPos = nextPosition (pos $ last snake) angle 

growSnake snake =
    (head snake:snake)

nextPosition position angle =
    case angle of
        Up ->
            (x, y + 1)
        Down ->
            (x, y - 1)
        Right ->
            (x + 1, y)
        Left ->
            (x - 1, y)

    where x = fst position
          y = snd position
        
initialSnake = [ snakePiece (0,0) Right
               , snakePiece (nextPosition (0,0) Right) Right]

rest snake =
    tail $ reverse snake
