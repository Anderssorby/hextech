module HexTech.Input
  ( module HexTech.Input
  , SDL.MouseButton(..)
  )
where

import qualified SDL
import           KeyState
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Lens
import           Data.Maybe                     ( mapMaybe )

import qualified HexTech.Engine.Types          as T
--import           HexTech.Wrapper.SDLInput       ( SDLInput(..)
--                                                , keycodeReleased
--                                                , keycodePressed
--                                                )


data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iLeft :: KeyState Int
  , iRight :: KeyState Int
  , iEscape :: KeyState Int
  , iSelect :: KeyState Int
  , iCtrl :: KeyState Int
  , iPause :: KeyState Int
  , iQuit :: Bool
  , iMute :: KeyState Int
  , iShowCoords :: KeyState Int
  , iMouseClick :: [MouseEvent]
  } deriving (Show, Eq)

data MouseEvent = MouseEvent {_mousePos :: T.Point, _mouseButton:: SDL.MouseButton } deriving (Show, Eq)
makeLenses ''MouseEvent

initInput :: Input
initInput = Input { iSpace      = initKeyState
                  , iUp         = initKeyState
                  , iDown       = initKeyState
                  , iLeft       = initKeyState
                  , iRight      = initKeyState
                  , iEscape     = initKeyState
                  , iSelect     = initKeyState
                  , iPause      = initKeyState
                  , iCtrl       = initKeyState
                  , iQuit       = False
                  , iMute       = initKeyState
                  , iShowCoords = initKeyState
                  , iMouseClick = []
                  }

keycodePressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodePressed keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData { keyboardEventKeysym = SDL.Keysym { keysymKeycode = code }, keyboardEventKeyMotion = motion, keyboardEventRepeat }
    -> code == keycode && motion == SDL.Pressed && not keyboardEventRepeat
  _ -> False

mouseButtonEvent :: SDL.EventPayload -> Maybe MouseEvent
mouseButtonEvent event = case event of
  SDL.MouseButtonEvent SDL.MouseButtonEventData { mouseButtonEventButton, mouseButtonEventPos }
    -> Just $ MouseEvent { _mousePos    = T.toPoint mouseButtonEventPos
                         , _mouseButton = mouseButtonEventButton
                         }
  _ -> Nothing

keycodeReleased :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodeReleased keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData { keyboardEventKeysym = SDL.Keysym { keysymKeycode = code }, keyboardEventKeyMotion = motion, keyboardEventRepeat }
    -> code == keycode && motion == SDL.Released && not keyboardEventRepeat
  _ -> False
--
--class Monad m => SDLInput m where
--  pollEventPayloads :: m [SDL.EventPayload]

pollEventPayloads :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads = liftIO $ map SDL.eventPayload <$> SDL.pollEvents

{-| This class describes type implementations that can handle user input and record it in a Monad
-}

class MonadIO m => HasInput m where
  updateInput :: m ()
  setInput :: Input -> m ()
  getInput :: m Input

updateInput' :: (HasInput m) => m ()
updateInput' = do
  input  <- getInput
  events <- pollEventPayloads
  setInput (stepControl events input)

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl events Input { iSpace, iUp, iDown, iEscape, iSelect, iMute, iShowCoords, iLeft, iRight, iCtrl, iPause }
  = Input { iSpace      = next 1 [SDL.KeycodeSpace] iSpace
          , iPause      = next 1 [SDL.KeycodeP] iPause
          , iUp         = next 1 [SDL.KeycodeUp, SDL.KeycodeW] iUp
          , iDown       = next 1 [SDL.KeycodeDown, SDL.KeycodeS] iDown
          , iLeft       = next 1 [SDL.KeycodeLeft, SDL.KeycodeA] iLeft
          , iRight      = next 1 [SDL.KeycodeRight, SDL.KeycodeD] iRight
          , iEscape     = next 1 [SDL.KeycodeEscape, SDL.KeycodeQ] iEscape
          , iSelect     = next 1 [SDL.KeycodeReturn, SDL.KeycodeSpace] iSelect
          , iCtrl       = next 1 [SDL.KeycodeLCtrl, SDL.KeycodeRCtrl] iCtrl
          , iMute       = next 1 [SDL.KeycodeM] iMute
          , iShowCoords = next 1 [SDL.KeycodeN] iShowCoords
          , iMouseClick = mapMaybe mouseButtonEvent events
          , iQuit       = elem SDL.QuitEvent events
          }
 where
  next count keycodes keystate | or $ map pressed keycodes = pressedKeyState
                               | or $ map released keycodes = releasedKeyState
                               | otherwise = maintainKeyState count keystate
  released keycode = or $ map (keycodeReleased keycode) events
  pressed keycode = or $ map (keycodePressed keycode) events
