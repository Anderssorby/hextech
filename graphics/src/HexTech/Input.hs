module HexTech.Input where

import qualified SDL
import           Control.Monad.State
import           KeyState

import           HexTech.Wrapper.SDLInput


data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  , iQuit :: Bool
  , iMute :: KeyState Int
  } deriving (Show, Eq)

initInput :: Input
initInput = Input { iSpace  = initKeyState
                  , iUp     = initKeyState
                  , iDown   = initKeyState
                  , iEscape = initKeyState
                  , iQuit   = False
                  , iMute   = initKeyState
                  }

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
stepControl events Input { iSpace, iUp, iDown, iEscape, iMute } = Input
  { iSpace  = next 1 [SDL.KeycodeSpace] iSpace
  , iUp     = next 1 [SDL.KeycodeUp, SDL.KeycodeW] iUp
  , iDown   = next 1 [SDL.KeycodeDown, SDL.KeycodeS] iDown
  , iEscape = next 1 [SDL.KeycodeEscape, SDL.KeycodeQ] iEscape
  , iMute   = next 1 [SDL.KeycodeM] iMute
  , iQuit   = elem SDL.QuitEvent events
  }
 where
  next count keycodes keystate | or $ map pressed keycodes = pressedKeyState
                               | or $ map released keycodes = releasedKeyState
                               | otherwise = maintainKeyState count keystate
  released keycode = or $ map (keycodeReleased keycode) events
  pressed keycode = or $ map (keycodePressed keycode) events
