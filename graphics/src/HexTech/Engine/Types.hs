module HexTech.Engine.Types where
import qualified Animate

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

newtype Seconds = Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()
