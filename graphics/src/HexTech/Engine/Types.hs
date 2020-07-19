module HexTech.Engine.Types where

import qualified Animate
import qualified SDL
import           Data.Text

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

import           Control.Monad.IO.Class         ( MonadIO(..) )

import           Data.Time.Clock.System         ( getSystemTime
                                                , systemSeconds
                                                )
import           System.Console.ANSI

newtype Point = Point (Int, Int) deriving (Show, Eq)

--instance Num Point where
--(Point (x1, y1)) * (Point (x2, y2)) = Point (x1 * x2, y1 * y2)
negateP (Point (x, y)) = p (-x) (-y)
  --abs (Point (x, y)) = sqrt $ fromIntegral x ^ 2 + fromIntegral y ^ 2
--instance Functor Point where
--  fmap f (Point (x, y)) = p (f x) (f y)

p_ :: Integral a => a -> a -> Point
p_ x y = Point (fromIntegral x, fromIntegral y)

p :: Int -> Int -> Point
p x y = p_ x y

(<+>) :: Point -> Point -> Point
(Point (x1, y1)) <+> (Point (x2, y2)) = Point (x1 + x2, y1 + y2)

(<->) :: Point -> Point -> Point
p1 <-> p2 = p1 <+> negateP p2

type SDLPoint a = SDL.Point SDL.V2 a

mkSDLPoint :: (Integral a, Integral b) => a -> a -> SDLPoint b
mkSDLPoint x y = SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)

toSDLPoint :: Integral a => Point -> SDLPoint a
toSDLPoint (Point (x, y)) = mkSDLPoint x y

toPoint :: Integral a => SDLPoint a -> Point
toPoint (SDL.P (SDL.V2 x y)) = p_ x y

frameDeltaSeconds :: Fractional a => a
frameDeltaSeconds = 0.016667

frameDeltaMilliseconds :: Int
frameDeltaMilliseconds = 16

newtype Seconds = Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

secondsToInteger :: Seconds -> Integer
secondsToInteger (Seconds f) = floor f

newtype Stocks = Stocks Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

newtype Percent = Percent Float
  deriving (Show, Eq, Num, Fractional, Floating, RealFrac, Real, Ord)

newtype Distance = Distance Float
  deriving (Show, Eq, Num, Fractional, RealFrac, Real, Ord)


newtype Score = Score Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

class Monad m => Clock m where
  delayMilliseconds :: Int -> m ()

showText :: Show a => a -> Text
showText = pack . show

class (MonadIO m, Monad m) => Logger m where
  logText :: Text -> m ()
  logShow :: Show a => a -> m ()
  logShow = logText . showText
  logInfo :: Show a => a -> m ()
  logInfo v = do
      liftIO $ setSGR [SetColor Foreground Vivid Cyan]
      sysTime <- liftIO getSystemTime
      logText ( "[" <> showText (systemSeconds sysTime) <> "] INFO: " <> showText v )
      liftIO $ setSGR [Reset]  -- Reset to default colour scheme
