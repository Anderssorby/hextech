module HexTech.Config
  ( Config(..)
  )
where

import qualified SDL
import           HexTech.Resource               ( Resources )

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }
