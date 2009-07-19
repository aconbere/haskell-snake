module Grid where

import Graphics.Rendering.OpenGL


gridSize = 40 
squareSize = (2 / gridSize)

get (x, y) =
    ((fromInteger x) * squareSize, (fromInteger y) * squareSize)
