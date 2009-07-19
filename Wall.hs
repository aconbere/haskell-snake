module Wall where

import Graphics.Rendering.OpenGL
import Shape
import Piece
import qualified Color
import Grid

wall position =
    Piece (Square 1) Color.Blue position

border =
    top ++ bottom ++ left ++ right
    where top = [wall (x, max) | x <- l]
          bottom = [wall (x, min) | x <- l]
          left = [wall (min, y) | y <- l]
          right = [wall (max, y) | y <- l]
          max = floor $ (gridSize / 2) -1
          min = - (floor $ gridSize / 2)
          l = [min .. max]
