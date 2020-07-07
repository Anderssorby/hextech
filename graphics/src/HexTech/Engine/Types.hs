{-# LANGUAGE NoImplicitPrelude #-}
module HexTech.Engine.Types where
import           Relude
import qualified Animate
import           Data.Text

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

--import           Control.Monad.IO.Class         ( MonadIO )

import           Data.Time.Clock.System         ( getSystemTime
                                                , systemSeconds
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

showText :: Show a => a -> Text
showText = pack . show

class (MonadIO m, Monad m) => Logger m where
  logText :: Text -> m ()
  logShow :: Show a => a -> m ()
  logShow = logText . showText
  logInfo :: Show a => a -> m ()
  logInfo v = do
      sysTime <- liftIO getSystemTime
      logText ( "[" <> showText (systemSeconds sysTime) <> "] INFO: " <> showText v )
