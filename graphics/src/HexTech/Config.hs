module HexTech.Config where

import qualified SDL
import           Linear
import           HexTech.Resource               ( Resources )

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }

screenWidth, screenHeight :: Float
screenWidth = 1280
screenHeight = 920

screenV2 :: V2 Float
screenV2 = V2 screenWidth screenHeight
