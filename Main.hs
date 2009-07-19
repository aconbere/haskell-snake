import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Monad.Random

import Bindings
import Display
import qualified Angle

import Snake
import Food
import Wall
import GameState
import Piece

main = do
    (progname,_) <- getArgsAndInitialize
    initialWindowSize $= Size 600 600
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Snake"

    angle <- newIORef $ Angle.Up
    snake <- newIORef $ initialSnake
    state <- newIORef $ Running
    score <- newIORef $ 0

    let walls = border
    wallList <- piecesList walls

    initialFood <- randomFoodConstrained initialSnake
    food <- newIORef $ initialFood

    displayCallback $= display snake walls wallList food state score
    keyboardMouseCallback $= Just (keyboardMouse angle state)
    idleCallback $= Just idle2
    reshapeCallback $= Just reshape
    addTimerCallback 150 (moveSnake snake angle food score state)

    mainLoop
