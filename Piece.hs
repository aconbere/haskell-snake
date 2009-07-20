module Piece where

import Graphics.Rendering.OpenGL hiding(Angle, Color, Red, Blue, Green)

import Shape
import Color
import Angle
import qualified Grid

data Piece = Piece { shape :: Shape
                   , col :: Color
                   , pos :: (Integer, Integer)
                   }
           | SnakePiece { shape :: Shape
                        , col :: Color
                        , pos :: (Integer, Integer)
                        , angle :: Angle
                        } deriving (Show, Eq)

intersect p1 p2 =
    pos p1 == pos p2

renderPiece piece =
    preservingMatrix $ do
        translate $ Vector3 (fst position) (snd position) 0
        renderShape (shape piece) (renderColor $ col piece)
    where position = Grid.get $ pos piece

renderPieceList piece =
    preservingMatrix $ do
        translate $ Vector3 (fst position) (snd position) 0
        renderShape (shape piece) (renderColor $ col piece)
    where position = Grid.get $ pos piece

renderPieces pieces =
    mapM_ renderPiece pieces

piecesList pieces =
    defineNewList Compile $ renderPieces pieces

renderPiecesList piecesList =
    callList piecesList
