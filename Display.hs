module Display where
import Data.IORef

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Text.Printf
import qualified List

import Piece
import Snake
import Wall
import Food
import GameState
import qualified Grid
import qualified Color


display snake walls wallList food state score = do
    clear [ColorBuffer]

    snake' <- get snake
    food' <- get food
    state' <- get state
    score' <- get score

    case state' of
        Won ->
            renderWonGame

        Lost ->
            renderLostGame score'

        Running ->
            if any (intersect $ last snake') (walls ++ (rest snake'))
                then
                    state $= Lost
                else
                    renderGame snake' wallList food'
        Paused ->
            preservingMatrix $ do
                renderGame snake' wallList food'
                notification $ printf "Paused"

    flush
    swapBuffers

--renderGame :: [Piece] -> DisplayList -> [Piece]
renderGame snake wallList food =
    preservingMatrix $ do
        renderPiecesList wallList
        renderPieces snake
        renderPiece food

renderPosition snake =
    notification $ printf "Pos: (%s,%s)" (show x) (show y)
    where x = fst $ pos (last snake)
          y = snd $ pos (last snake)

renderLostGame score = 
    notification $ printf "GAME OVER -- YOUR SCORE: %s" $ show score

renderWonGame = 
    notification $ printf "YOU WIN!"

notification statement =
    preservingMatrix $ do
        color $ Color.renderColor Color.Green
        translate $ Vector3 (-0.5::GLfloat) 0 0
        scale 0.0004 0.0004 (0.0004::GLfloat)
        renderString MonoRoman $ statement

idle2 = do
   postRedisplay Nothing

moveSnake snake angle food score state = do
    state' <- get state
    snake' <- get snake
    angle' <- get angle
    food' <- get food
    score' <- get score

    case state' of
        Running -> do
            let newSnake = nextSnake snake' angle'

            case intersect (last newSnake) food' of
                 True -> do
                    snake $= growSnake newSnake
                    newFood <- randomFoodConstrained newSnake
                    food $= newFood
                    score $= score' + 1
                 False ->  do
                    snake $= newSnake
        _ ->
            snake $= snake'
                    
    addTimerCallback 150 (moveSnake snake angle food score state)
