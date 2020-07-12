module HexTech.Input where

import qualified SDL
import           KeyState

import           HexTech.Wrapper.SDLInput       ( SDLInput(..)
                                                , keycodeReleased
                                                , keycodePressed
                                                )


data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iLeft :: KeyState Int
  , iRight :: KeyState Int
  , iEscape :: KeyState Int
  , iEnter :: KeyState Int
  , iCtrl :: KeyState Int
  , iQuit :: Bool
  , iMute :: KeyState Int
  , iShowCoords :: KeyState Int
  } deriving (Show, Eq)

initInput :: Input
initInput = Input { iSpace      = initKeyState
                  , iUp         = initKeyState
                  , iDown       = initKeyState
                  , iLeft       = initKeyState
                  , iRight      = initKeyState
                  , iEscape     = initKeyState
                  , iEnter      = initKeyState
                  , iCtrl       = initKeyState
                  , iQuit       = False
                  , iMute       = initKeyState
                  , iShowCoords = initKeyState
                  --, iMouseMove  =
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
stepControl events Input { iSpace, iUp, iDown, iEscape, iEnter, iMute, iShowCoords, iLeft, iRight, iCtrl }
  = Input { iSpace      = next 1 [SDL.KeycodeSpace, SDL.KeycodeP] iSpace
          , iUp         = next 1 [SDL.KeycodeUp, SDL.KeycodeW] iUp
          , iDown       = next 1 [SDL.KeycodeDown, SDL.KeycodeS] iDown
          , iLeft       = next 1 [SDL.KeycodeLeft, SDL.KeycodeA] iLeft
          , iRight      = next 1 [SDL.KeycodeRight, SDL.KeycodeD] iRight
          , iEscape     = next 1 [SDL.KeycodeEscape, SDL.KeycodeQ] iEscape
          , iEnter      = next 1 [SDL.KeycodeReturn] iEnter
          , iCtrl       = next 1 [SDL.KeycodeLCtrl, SDL.KeycodeRCtrl] iCtrl
          , iMute       = next 1 [SDL.KeycodeM] iMute
          , iShowCoords = next 1 [SDL.KeycodeN] iShowCoords
          --, iMouseMove = next 1 [SDL.MouseMotionEvent] iMouseMove
          , iQuit       = elem SDL.QuitEvent events
          }
 where
  next count keycodes keystate | or $ map pressed keycodes = pressedKeyState
                               | or $ map released keycodes = releasedKeyState
                               | otherwise = maintainKeyState count keystate
  released keycode = or $ map (keycodeReleased keycode) events
  pressed keycode = or $ map (keycodePressed keycode) events
