module HexTech.Camera where

import           Linear

import           Control.Lens
import qualified SDL
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.StateVar                  ( ($=) )
import           SDL.Vect



moveCamera :: MonadIO m => SDL.Renderer -> Camera -> m ()
moveCamera renderer Camera { camZoom, camOrigin } = do
  SDL.rendererScale renderer $= (fmap realToFrac camZoom)
  let dim = fmap truncate $ screenV2
  SDL.rendererViewport renderer
    $= ( Just
       $ SDL.Rectangle (SDL.P $ (fmap truncate $ moveOrigin camOrigin)) dim
       )
  SDL.rendererClipRect renderer $= (Just $ SDL.Rectangle (SDL.P $ V2 0 0) dim)

data Camera = Camera
  { camOrigin :: V2 Float
  , camZoom :: V2 Float
  } deriving (Show, Eq)

--makeClassy_ ''Camera

class Monad m => CameraControl m where
  adjustCamera :: Camera -> m ()
  disableZoom :: m ()
  enableZoom :: m ()

screenWidth, screenHeight :: Float
screenWidth = 1280
screenHeight = 720

screenV2 :: V2 Float
screenV2 = V2 screenWidth screenHeight

initCamera :: Camera
initCamera = Camera (V2 (screenWidth / 2) (screenHeight / 2)) (V2 1 1)

moveOrigin :: V2 Float -> V2 Float
moveOrigin (V2 x y) = V2 (screenWidth / 2 - x) (screenHeight / 2 - y)

lerpCamera :: Float -> Camera -> Camera -> Camera
lerpCamera p a b =
  Camera (lerp p (camOrigin a) (camOrigin b)) (lerp p (camZoom a) (camZoom b))
