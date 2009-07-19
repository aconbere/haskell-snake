module Food where
import Control.Monad.Random

import Piece
import Shape
import Color
import Grid

food position =
    Piece (Square 1) Green position

cord :: (RandomGen g) => Rand g Integer
cord = getRandomR (min, max)
    where max = (floor $ gridSize / 2) - 2
          min = -max 

randomFood = do
    x <- evalRandIO cord
    y <- evalRandIO cord
    return $ food (x, y)

randomFoodConstrained const = do
    f <- randomFood

    if any (intersect f) const
        then
            randomFoodConstrained const
        else
            return f
    
