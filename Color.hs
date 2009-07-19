module Color where
import Graphics.Rendering.OpenGL hiding (Red, Green, Blue, Color)

data Color = Red
           | Green
           | Blue
           | Yellow
           | Violet
           | Teal
           | White
           | Color GLfloat GLfloat GLfloat
           deriving (Show, Eq)

renderColor color =
    case color of
        Red    -> Color3 (1.0::GLfloat) 0.0 0.0
        Green  -> Color3 (0.0::GLfloat) 1.0 0.0
        Blue   -> Color3 (0.0::GLfloat) 0.0 1.0
        Yellow -> Color3 (1.0::GLfloat) 1.0 0.0
        Violet -> Color3 (1.0::GLfloat) 1.0 0.0
        Teal   -> Color3 (1.0::GLfloat) 0.0 1.0
        White  -> Color3 (1.0::GLfloat) 1.0 1.0
        (Color r g b) -> Color3 r g b
