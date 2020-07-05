module HexTech.Input where

import qualified SDL
import           Control.Monad.State
import           KeyState

--import           HexTech.Engine.Input
import           HexTech.Wrapper.SDLInput
--import           HexTech.State

import           KeyState

data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  , iQuit :: Bool
  } deriving (Show, Eq)

initInput :: Input
initInput = Input initKeyState initKeyState initKeyState initKeyState False

{-| This class describes type implementations that can handle user input and record it in a Monad
-}

class Monad m => HasInput m where
  updateInput :: m ()
  setInput :: Input -> m ()
  getInput :: m Input

updateInput' :: (HasInput m, SDLInput m) => m ()
updateInput' = do
  input  <- getInput
  events <- pollEventPayloads
  setInput (stepControl events input)

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl events Input { iSpace, iUp, iDown, iEscape } = Input
  { iSpace  = next 1 [SDL.KeycodeSpace] iSpace
  , iUp     = next 1 [SDL.KeycodeUp, SDL.KeycodeW] iUp
  , iDown   = next 1 [SDL.KeycodeDown, SDL.KeycodeS] iDown
  , iEscape = next 1 [SDL.KeycodeEscape] iEscape
  , iQuit   = elem SDL.QuitEvent events
  }
 where
  next count keycodes keystate | or $ map pressed keycodes = pressedKeyState
                               | or $ map released keycodes = releasedKeyState
                               | otherwise = maintainKeyState count keystate
  released keycode = or $ map (keycodeReleased keycode) events
  pressed keycode = or $ map (keycodePressed keycode) events
