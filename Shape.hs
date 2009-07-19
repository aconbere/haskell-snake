module Shape where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import qualified Grid

data Shape = Square Integer deriving (Show, Eq)

renderShape (Square w) c = do
    renderQuad w w c

renderQuad h w c = do 
    color $ c
    renderPrimitiveQuad h w

renderPrimitiveQuad h w = do
    renderPrimitive Quads $ do
        vertex $ Vertex3 (0::GLfloat) 0 0
        vertex $ Vertex3 0 x 0
        vertex $ Vertex3 y x 0
        vertex $ Vertex3 y 0 0
    where (x, y) = Grid.get (w, h)

renderPrimitiveQuadList h w =
    defineNewList Compile $ renderPrimitiveQuad h w
