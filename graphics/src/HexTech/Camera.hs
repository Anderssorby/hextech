module HexTech.Camera where

import           Linear

import           Control.Lens                   ( makeClassy )
import qualified SDL
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.StateVar                  ( ($=) )
import           HexTech.Config                 ( screenHeight
                                                , screenWidth
                                                , screenV2
                                                )


data Camera = Camera
  { camOrigin :: V2 Float
  , camZoom :: V2 Float
  } deriving (Show, Eq)

makeClassy ''Camera

moveCamera :: MonadIO m => SDL.Renderer -> Camera -> m ()
moveCamera renderer Camera { camZoom, camOrigin } = do
  SDL.rendererScale renderer $= (fmap realToFrac camZoom)
  let dim = fmap truncate $ screenV2
  SDL.rendererViewport renderer
    $= ( Just
       $ SDL.Rectangle (SDL.P $ (fmap truncate $ moveOrigin camOrigin)) dim
       )
  SDL.rendererClipRect renderer $= (Just $ SDL.Rectangle (SDL.P $ V2 0 0) dim)


class Monad m => CameraControl m where
  adjustCamera :: Camera -> m ()
  disableZoom :: m ()
  enableZoom :: m ()

initCamera :: Camera
initCamera = Camera (V2 (screenWidth / 2) (screenHeight / 2)) (V2 1 1)

moveOrigin :: V2 Float -> V2 Float
moveOrigin (V2 x y) = V2 (screenWidth / 2 - x) (screenHeight / 2 - y)

lerpCamera :: Float -> Camera -> Camera -> Camera
lerpCamera p a b =
  Camera (lerp p (camOrigin a) (camOrigin b)) (lerp p (camZoom a) (camZoom b))
