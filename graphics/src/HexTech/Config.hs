module HexTech.Config
  ( Config(..)
  , Resources(..)
  )
where

import qualified SDL

import           HexTech.Resource

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }
