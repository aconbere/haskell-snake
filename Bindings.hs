module Bindings (reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Prelude
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import qualified Angle
import GameState

reshape s@(Size _w _h) = do
    viewport $= (Position 0 0, s)
    postRedisplay Nothing

keyboardAct angle _gameState (SpecialKey KeyLeft) Down = do
    angle' <- get angle
    angle $= Angle.noOpposits angle' Angle.Left

keyboardAct angle _gameState (SpecialKey KeyRight) Down = do
    angle' <- get angle
    angle $= Angle.noOpposits angle' Angle.Right

keyboardAct angle _gameState (SpecialKey KeyUp) Down = do
    angle' <- get angle
    angle $= Angle.noOpposits angle' Angle.Up

keyboardAct angle _gameState (SpecialKey KeyDown) Down = do
    angle' <- get angle
    angle $= Angle.noOpposits angle' Angle.Down

keyboardAct _angle _gameState (Char '\27') Down = do
    exitWith ExitSuccess

keyboardAct _angle gameState (Char ' ') Down = do
    state <- get gameState
    case state of
        Paused ->
            gameState $= Running
        Running ->
            gameState $= Paused
        _ ->
            gameState $= state

keyboardAct _ _ _ _ = return ()

keyboardMouse angle gameState key state _modifiers _position = do
    keyboardAct angle gameState key state
