module HexTech.Engine.Renderer where

import qualified Animate
import qualified SDL
import           Data.StateVar                  ( ($=) )
import qualified Data.StateVar                 as StateVar
import           Foreign.C.Types
import           Data.Word                      ( Word8 )
import           SDL.Vect                       ( V2(..)
                                                , V4(..)
                                                , (*^)
                                                )
import           Control.Monad.Reader
import qualified Data.Vector.Storable          as Vector
import           Data.Vector.Storable           ( Vector )

import           HexTech.Config                 ( Config(..) )
import           HexTech.Resource               ( toDigitReverse
                                                , Digit(..)
                                                , Resources(..)
                                                , CommanderKey
                                                )
import           HexTech.Engine.Types
import           HexTech.Wrapper.SDLRenderer    ( SDLRenderer(..) )
import           HexTech.Grid                   ( Grid(..)
                                                , tileCoords
                                                , getAxialCoords
                                                )

class (Monad m
      , SDLRenderer m
      , MonadReader Config m
      , MonadIO m
      , Logger m
      ) => Renderer m where

  clearScreen :: m ()
  drawScreen :: m ()
  drawGameOverText :: (Int, Int) -> m ()
  drawPressSpaceText :: (Int, Int) -> m ()
  drawPressEscapeText :: (Int, Int) -> m ()
  drawTitleText :: (Int, Int) -> m ()
  drawControlsText :: (Int, Int) -> m ()

clearScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
clearScreen' = do
  renderer <- asks cRenderer
  clearRenderer renderer

drawScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
drawScreen' = do
  renderer <- asks cRenderer
  presentRenderer renderer

data Color = White | Red | Blue | Green | Yellow deriving (Eq, Enum, Show)
type ColorV4 = SDL.V4 Word8


colorV4 :: Color -> SDL.V4 Word8
colorV4 White  = SDL.V4 maxBound maxBound maxBound maxBound
colorV4 Red    = SDL.V4 maxBound 0 0 maxBound
colorV4 Green  = SDL.V4 0 maxBound 0 maxBound
colorV4 Blue   = SDL.V4 0 0 maxBound maxBound
colorV4 Yellow = SDL.V4 maxBound maxBound 0 maxBound

getColorV4 :: (Renderer m) => m ColorV4
getColorV4 = do
  r <- asks (cRenderer)
  StateVar.get $ SDL.rendererDrawColor r

setColor :: Renderer m => Color -> m ()
setColor = setColorV4 . colorV4

setColorV4 :: Renderer m => ColorV4 -> m ()
setColorV4 c = do
  r <- asks (cRenderer)
  SDL.rendererDrawColor r $= c
--

mountainY, jungleY, groundY, riverY :: Int
mountainY = -16
jungleY = 16 * 0
groundY = 16 * 28
riverY = 16 * 36

--
mkPoint :: (Integral a, Integral b) => a -> a -> SDL.Point SDL.V2 b
mkPoint x y = SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)

drawLine :: (MonadIO m) => SDL.Renderer -> (Int, Int) -> (Int, Int) -> m ()
drawLine r (ox, oy) (tx, ty) = SDL.drawLine r (mkPoint ox oy) (mkPoint tx ty)

drawLines
  :: (MonadIO m, MonadReader Config m) => Vector (SDL.Point SDL.V2 CInt) -> m ()
drawLines points = do
  renderer <- asks cRenderer
  SDL.drawLines renderer points

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
  :: (Renderer m)
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
  :: (Renderer m)
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

drawBlackOverlay
  :: (MonadReader Config m, SDLRenderer m, MonadIO m) => Percent -> m ()
drawBlackOverlay (Percent percent) = do
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

drawDigit
  :: (SDLRenderer m, Renderer m, MonadReader Config m)
  => Digit
  -> (Int, Int)
  -> m ()
drawDigit d = drawTextureSprite (flip rDigitSprites d . cResources)

drawDigits
  :: (SDLRenderer m, Renderer m, MonadReader Config m)
  => Integer
  -> (Int, Int)
  -> m ()
drawDigits int (x, y) = mapM_ (\(i, d) -> drawDigit d (x - i * 16, y))
                              (zip [0 ..] (toDigitReverse int))
--stepHorizontalDistance :: Distance -> Distance -> Distance
--stepHorizontalDistance dist speed = if dist' <= -1280
--  then dist' + 1280
--  else dist'
--  where dist' = dist + speed

drawCommander
  :: Renderer m => Animate.SpriteClip CommanderKey -> (Int, Int) -> m ()
drawCommander = drawSprite (rCommanderSprites . cResources)

drawJungle :: Renderer m => (Int, Int) -> m ()
drawJungle = drawHorizontalScrollImage (rJungleSprites . cResources) 8

drawGround :: Renderer m => (Int, Int) -> m ()
drawGround = drawHorizontalScrollImage (rGroundSprites . cResources) 2

gridToPixels :: Grid -> (Int, Int) -> [Vector (SDL.Point SDL.V2 CInt)]
gridToPixels grid (gx, gy) = do
  let tiles = gridTiles grid
  row  <- tiles
  tile <- row
  let
    (x, y)    = getAxialCoords $ tileCoords tile
    lineWidth = 0
    size      = 50 :: Float
    xOffset   = sqrt 3 * size :: Float
    yOffset   = 1.5 * size :: Float
    py =
      round $ fromIntegral x + yOffset * fromIntegral y - 2 * lineWidth :: Int
    px =
      ( round
      $ fromIntegral x
      + xOffset
      * (fromIntegral x + 0.5 * fromIntegral (y `rem` 2 :: Int))
      - 2
      * lineWidth
      ) :: Int

  return $ makeHexagon (gx + px, gy + py) $ round size

type Point = (Int, Int)

makeHexagon :: Point -> Int -> Vector (SDL.Point SDL.V2 CInt)
makeHexagon center size = Vector.generate 7 $ pointyHexCorner center size

pointyHexCorner :: Point -> Int -> Int -> SDL.Point SDL.V2 CInt
pointyHexCorner (x, y) size i = mkPoint px py
 where
  angleRad = pi / 3.0 * fromIntegral i + pi / 6.0 :: Double
  px       = round (fromIntegral x + fromIntegral size * cos (angleRad)) :: CInt
  py       = round (fromIntegral y + fromIntegral size * sin (angleRad)) :: CInt
