module Angle where

import Graphics.Rendering.OpenGL
import Prelude hiding (Left, Right)

data Angle = Left | Right | Up | Down deriving (Show, Eq)

opposite a =
    case a of
        Left -> Right
        Right -> Left
        Up -> Down
        Down -> Up

isOpposite a a' =
    opposite a == a'

-- if b is the opposite of a then ignore b
-- otherwise go in that direction
noOpposits a b =
    if isOpposite a b 
        then a
        else b
