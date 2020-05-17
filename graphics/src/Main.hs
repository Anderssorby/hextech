{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main
  ( main
  )
where

import qualified SDL
import           SDL                            ( ($=) )
import qualified Common                        as C

import           Control.Monad                  ( foldM )
import           Control.Monad.Extra            ( whileM )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Vector.Storable          as Vector
import           Data.Vector.Storable           ( Vector )
import           Prelude                 hiding ( Left
                                                , Right
                                                )

import           Foreign.C.Types                ( CInt(..) )

import           Graphics
-- For debugging
--import qualified Debug.Trace                   as Debug

data Intent
  = SelectSurface Direction
  | Idle
  | Quit


data Direction
  = Help
  | Up
  | Down
  | Left
  | Right


data SurfaceMap a = SurfaceMap
  { help  :: a
  , up    :: a
  , down  :: a
  , left  :: a
  , right :: a
  } deriving (Foldable, Traversable, Functor)

data Color = White | Red | Blue | Green | Yellow

type Point t = (t, t)

surfacePaths :: SurfaceMap FilePath
surfacePaths = SurfaceMap { help  = "./assets/press.bmp"
                          , up    = "./assets/up.bmp"
                          , down  = "./assets/down.bmp"
                          , left  = "./assets/left.bmp"
                          , right = "./assets/right.bmp"
                          }


main :: IO ()
main = makeWindow

{-| Scale a surface to another surface
-}
renderScaled
  :: (MonadIO m) => SDL.Renderer -> SDL.Surface -> SDL.Surface -> m ()
renderScaled r s t = SDL.surfaceBlitScaled t Nothing s Nothing
  -- >> SDL.present r


{-| Convert an SDL event to an Intent
-}
mkIntent :: Maybe SDL.Event -> Intent
mkIntent = maybe Idle (payloadToIntent . extractPayload)


extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

{-| Convert an event payload like a keyboard event to an intent
-}
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Idle


{-| Map a keyboard event to an intent
-}
getKey :: SDL.KeyboardEventData -> Intent
getKey (SDL.KeyboardEventData _ SDL.Released _    _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed  True _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeQ      -> Quit
    SDL.KeycodeUp     -> SelectSurface Up
    SDL.KeycodeDown   -> SelectSurface Down
    SDL.KeycodeLeft   -> SelectSurface Left
    SDL.KeycodeRight  -> SelectSurface Right
    _                 -> SelectSurface Help


{-| Update the system from the intent
-}
runIntent :: (Monad m) => SurfaceMap a -> (a -> m ()) -> Intent -> m Bool
runIntent _  _ Quit                = pure False

runIntent _  _ Idle                = pure True

runIntent cs f (SelectSurface key) = True <$ f (selectSurface key cs)


selectSurface :: Direction -> SurfaceMap a -> a
selectSurface Help  = help
selectSurface Up    = up
selectSurface Down  = down
selectSurface Left  = left
selectSurface Right = right

--instance (Vector.Storable a) => Foldable (Vector a) where
--    foldMap :: (a -> b) -> Vector a -> Vector b
--    foldMap f v = Vector.map f v

makeGrid :: [Vector (SDL.Point SDL.V2 CInt)]
makeGrid = map tile coords
 where
  rows      = 10
  cols      = 10
  (x, y)    = (50, 50) :: (Float, Float)
  lineWidth = 0
  size      = 50 :: Float
  xOffset   = sqrt 3 * size :: Float
  yOffset   = 1.5 * size :: Float
  tile (i, j) =
    makeHexagon
        ( round
        $ x
        + xOffset
        * (j + 0.5 * fromIntegral (i `rem` 2 :: Int))
        - 2
        * lineWidth
        , round $ y + yOffset * fromIntegral i - 2 * lineWidth
        )
      $ round size
  coords   = [ (i, j) | i <- [0 .. rows], j <- [0 .. cols] ]
  n_coords = length coords

{-| Draw view
-}
draw :: SDL.Renderer -> IO ()
draw r = do
  clearScreen r
  --withColor Red    >> fillRectangle' innerRect
  foldM (\_ hexagon -> (withColor Green >> (drawLines r $ hexagon))) ()
    $ makeGrid
  --withColor Blue   >> drawLine' (0, screenHeight `div` 2) (screenWidth, screenHeight `div` 2)
  --withColor Yellow >> mapM_ (\y -> drawDot' (screenWidth `div` 2, y)) [ 0, 4 .. screenHeight ]
  SDL.present r

 where
  innerRect = mkRect (screenWidth `div` 4)
                     (screenHeight `div` 4)
                     (screenWidth `div` 2)
                     (screenHeight `div` 2)
  outerRect = mkRect (screenWidth `div` 6)
                     (screenHeight `div` 6)
                     (2 * screenWidth `div` 3)
                     (2 * screenHeight `div` 3)
  withColor    = setColor r :: MonadIO m => Color -> m ()
  --fillRectangle' = fillRectangle r
  --drawRectangle' = drawRectangle r
  --drawLine' = drawLine r
  --drawDot' = drawDot r
  screenWidth  = 640
  screenHeight = 480


mkRect :: t -> t -> t -> t -> SDL.Rectangle t
mkRect x y w h = SDL.Rectangle o s
 where
  o = SDL.P (SDL.V2 x y)
  s = SDL.V2 w h

makeHexagon :: Point Int -> Int -> Vector (SDL.Point SDL.V2 CInt)
makeHexagon center size = Vector.generate 7 $ pointyHexCorner center size

pointyHexCorner :: Point Int -> Int -> Int -> SDL.Point SDL.V2 CInt
pointyHexCorner (x, y) size i = C.mkPoint px py
 where
  angleRad = pi / 3 * fromIntegral i + pi / 6
  px       = round (fromIntegral x + fromIntegral size * cos (angleRad)) :: CInt
  py       = round (fromIntegral y + fromIntegral size * sin (angleRad)) :: CInt


clearScreen :: (MonadIO m) => SDL.Renderer -> m ()
clearScreen r = do
  setColor r White
  SDL.clear r


fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)


drawRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
drawRectangle r s = SDL.drawRect r (Just s)


drawLine :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> m ()
drawLine r (ox, oy) (tx, ty) =
  SDL.drawLine r (C.mkPoint ox oy) (C.mkPoint tx ty)

drawLines
  :: (MonadIO m) => SDL.Renderer -> Vector (SDL.Point SDL.V2 CInt) -> m ()
drawLines r points = SDL.drawLines r points

drawDot :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> m ()
drawDot r (x, y) = SDL.drawPoint r (SDL.P (SDL.V2 x y))


setColor :: (MonadIO m) => SDL.Renderer -> Color -> m ()
setColor r White =
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red   = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue  = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Yellow =
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
