module HexTech.Engine.Types where
import qualified Animate
import           Data.Text

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

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


class Monad m => Logger m where
  logText :: Text -> m ()
