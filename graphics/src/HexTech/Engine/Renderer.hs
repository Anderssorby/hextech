module HexTech.Engine.Renderer where

import qualified Animate
import qualified SDL
import           Data.StateVar                  ( ($=) )
import           Foreign.C.Types
import           SDL.Vect
import           Control.Monad.Reader

import           HexTech.Config
import           HexTech.Engine.Types
import           HexTech.Wrapper.SDLRenderer    ( SDLRenderer(..) )

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()
  drawGameOverText :: (Int, Int) -> m ()
  drawPressSpaceText :: (Int, Int) -> m ()
  drawPressEscapeText :: (Int, Int) -> m ()
  drawTitleText :: (Int, Int) -> m ()
  --drawNumber :: Number -> (Int, Int) -> m ()
  drawControlsText :: (Int, Int) -> m ()

clearScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
clearScreen' = do
  renderer <- asks cRenderer
  clearRenderer renderer

drawScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
drawScreen' = do
  renderer <- asks cRenderer
  presentRenderer renderer

--

mountainY, jungleY, groundY, riverY :: Int
mountainY = -16
jungleY = 16 * 0
groundY = 16 * 28
riverY = 16 * 36

--

drawTextureSprite
  :: (SDLRenderer m, MonadReader Config m)
  => (Config -> SDL.Texture)
  -> (Int, Int)
  -> m ()
drawTextureSprite getTex (x, y) = do
  renderer <- asks cRenderer
  tex      <- asks getTex
  SDL.TextureInfo { textureWidth, textureHeight } <- queryTexture tex
  let dim = V2 textureWidth textureHeight
  drawTexture
    renderer
    tex
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim
    )

drawSprite
  :: (SDLRenderer m, MonadReader Config m)
  => (Config -> Animate.SpriteSheet key SDL.Texture Seconds)
  -> Animate.SpriteClip key
  -> (Int, Int)
  -> m ()
drawSprite ss clip (x, y) = do
  renderer <- asks cRenderer
  sheet    <- asks (Animate.ssImage . ss)
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  drawTexture
    renderer
    sheet
    (Just clip')
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim
    )

drawHorizontalScrollSprite
  :: (MonadReader Config m, SDLRenderer m)
  => (Config -> Animate.SpriteSheet key SDL.Texture Seconds)
  -> Int
  -> Animate.SpriteClip key
  -> (Int, Int)
  -> m ()
drawHorizontalScrollSprite ss scale clip (x, y) = do
  renderer <- asks cRenderer
  sheet    <- asks (Animate.ssImage . ss)
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  let dim'                        = fromIntegral scale *^ dim
  drawTexture
    renderer
    sheet
    (Just clip')
    (Just $ SDL.Rectangle
      (SDL.P $ SDL.V2 (fromIntegral x - 1280) (fromIntegral y))
      dim'
    )
  drawTexture
    renderer
    sheet
    (Just clip')
    ( Just
    $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim'
    )
  drawTexture
    renderer
    sheet
    (Just clip')
    (Just $ SDL.Rectangle
      (SDL.P $ SDL.V2 (fromIntegral x + 1280) (fromIntegral y))
      dim'
    )

getSpriteAnimations
  :: (MonadReader Config m)
  => (Config -> Animate.SpriteSheet key SDL.Texture Seconds)
  -> m (Animations key)
getSpriteAnimations ss = asks (Animate.ssAnimations . ss)

drawHorizontalScrollImage
  :: (MonadReader Config m, SDLRenderer m)
  => (Config -> SDL.Texture)
  -> Int
  -> (Int, Int)
  -> m ()
drawHorizontalScrollImage getTex scale (x, y) = do
  renderer <- asks cRenderer
  tex      <- asks getTex
  SDL.TextureInfo { textureWidth, textureHeight } <- queryTexture tex
  let dim  = SDL.V2 textureWidth textureHeight
  let dim' = fromIntegral scale *^ dim
  drawTexture
    renderer
    tex
    Nothing
    (Just $ SDL.Rectangle
      (SDL.P $ SDL.V2 (fromIntegral x - 1280) (fromIntegral y))
      dim'
    )
  drawTexture
    renderer
    tex
    Nothing
    ( Just
    $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim'
    )
  drawTexture
    renderer
    tex
    Nothing
    (Just $ SDL.Rectangle
      (SDL.P $ SDL.V2 (fromIntegral x + 1280) (fromIntegral y))
      dim'
    )

drawBlackOverlay'
  :: (MonadReader Config m, SDLRenderer m, MonadIO m) => Percent -> m ()
drawBlackOverlay' (Percent percent) = do
  renderer <- asks cRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.rendererDrawColor renderer $= (V4 0 0 0 (truncate $ 255 * percent))
  SDL.fillRect renderer Nothing
  SDL.rendererDrawBlendMode renderer $= SDL.BlendNone

--

rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip { scX, scY, scW, scH } = SDL.Rectangle
  (SDL.P (V2 (num scX) (num scY)))
  (V2 (num scW) (num scH))
  where num = fromIntegral

--stepHorizontalDistance :: Distance -> Distance -> Distance
--stepHorizontalDistance dist speed = if dist' <= -1280
--  then dist' + 1280
--  else dist'
--  where dist' = dist + speed
