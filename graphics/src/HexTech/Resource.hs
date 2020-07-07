module HexTech.Resource where

import qualified SDL
import qualified SDL.Mixer                     as Mixer
import qualified SDL.Font                      as Font
import qualified SDL.Image                     as Image
import qualified Animate
import           Data.StateVar                  ( ($=) )
import           SDL.Vect
import qualified SDL.Raw.Video                 as Raw
import qualified SDL.Internal.Numbered         as Numbered

import           HexTech.Engine.Types           ( showText )

data Resources = Resources
  { --rMountainSprites :: Animate.SpriteSheet MountainKey SDL.Texture Seconds
  --, rRiverSprites :: Animate.SpriteSheet RiverKey SDL.Texture Seconds
   rJungleSprites :: SDL.Texture
  , rGroundSprites :: SDL.Texture
  --, rDinoSprites :: Animate.SpriteSheet DinoKey SDL.Texture Seconds
  --, rBirdSprites :: Animate.SpriteSheet BirdKey SDL.Texture Seconds
  --, rLavaSprites :: Animate.SpriteSheet LavaKey SDL.Texture Seconds
  --, rRockSprites :: Animate.SpriteSheet RockKey SDL.Texture Seconds
  , rGameMusic :: Mixer.Music
  --, rJumpSfx :: Mixer.Chunk
  --, rDuckSfx :: Mixer.Chunk
  --, rPointSfx :: Mixer.Chunk
  --, rBirdSfx :: Mixer.Chunk
  --, rHurtSfx :: Mixer.Chunk
  --, rLavaSfx :: Mixer.Chunk
  --, rQuakeSfx :: Mixer.Chunk
  --, rRockSfx :: Mixer.Chunk
  --, rDeathSfx :: Mixer.Chunk
  --, rRecoverSfx :: Mixer.Chunk
  --, rStockSfx :: Mixer.Chunk
  , rMuted :: SDL.Texture
  , rUnMuted :: SDL.Texture
  , rPauseSprite :: SDL.Texture
  , rSpaceSprite :: SDL.Texture
  , rEscapeSprite :: SDL.Texture
  , rGameOverSprite :: SDL.Texture
  , rHiscoreSprite :: SDL.Texture
  , rTitleSprite :: SDL.Texture
  , rDigitSprites :: Digit -> SDL.Texture
  , rControlsSprite :: SDL.Texture
  }

-- | Produce a new 'SDL.Surface' based on an existing one, but
-- optimized for blitting to the specified 'SDL.PixelFormat'.
convertSurface :: SDL.Surface -> SDL.PixelFormat -> IO SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt     <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface0 <- Image.load path
  surface  <- convertSurface surface0 SDL.RGBA8888
  SDL.freeSurface surface0
  case alpha of
    Just (r, g, b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing        -> return ()
  return surface

alphaColorDef :: Animate.Color
alphaColorDef = (0xff, 0x00, 0xff)

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  smallFont      <- Font.load "resource/Computer Speak v0.3.ttf" 24
  bigFont        <- Font.load "resource/Computer Speak v0.3.ttf" 64
  titleFont      <- Font.load "resource/Computer Speak v0.3.ttf" 100
  gameMusic      <- Mixer.load "resource/v42.mod"
  --jumpSfx        <- Mixer.load "resource/jump.wav"
  --pointSfx       <- Mixer.load "resource/point.wav"
  --birdSfx        <- Mixer.load "resource/bird.wav"
  --duckSfx        <- Mixer.load "resource/duck.wav"
  --hurtSfx        <- Mixer.load "resource/hurt.wav"
  --quakeSfx       <- Mixer.load "resource/quake.wav"
  --rockSfx        <- Mixer.load "resource/rock.wav"
  --lavaSfx        <- Mixer.load "resource/lava.wav"
  --deathSfx       <- Mixer.load "resource/death.wav"
  --stockSfx       <- Mixer.load "resource/stock.wav"
  --recoverSfx     <- Mixer.load "resource/recover.wav"
  --mountainSprites <-
  --  Animate.readSpriteSheetJSON loadTexture "resource/mountain.json" :: IO
  --    (Animate.SpriteSheet MountainKey SDL.Texture Seconds)
  jungle         <- loadTexture "resource/jungle.png" (Just alphaColorDef)
  ground         <- loadTexture "resource/ground.png" (Just alphaColorDef)
  controlsSprite <- loadTexture "resource/controls.png" (Just alphaColorDef)
  --riverSprites   <-
  --  Animate.readSpriteSheetJSON loadTexture "resource/river.json" :: IO
  --    (Animate.SpriteSheet RiverKey SDL.Texture Seconds)
  pauseSprite <- toTexture =<< Font.solid bigFont (V4 255 255 255 255) "PAUSED"
  spaceSprite    <- toTexture
    =<< Font.solid smallFont (V4 255 255 255 255) "PRESS SPACE"
  escapeSprite <- toTexture
    =<< Font.solid smallFont (V4 255 255 255 255) "PRESS ESCAPE TO QUIT"
  gameOverSprite <- toTexture
    =<< Font.solid bigFont (V4 255 255 255 255) "GAME OVER"
  hiscoreSprite <- toTexture
    =<< Font.solid smallFont (V4 255 255 255 255) "HISCORE"
  muted <- toTexture =<< Font.solid smallFont (V4 255 255 255 255) "MUSIC OFF"
  unMuted <- toTexture =<< Font.solid smallFont (V4 255 255 255 255) "MUSIC ON"
  titleSprite <- toTexture =<< Font.shaded titleFont
                                           (V4 0xff 0xff 0xff 0xff)
                                           (V4 0x11 0x08 0x1e 0x2a)
                                           " Hex Tech "
  let drawDigit :: Int -> IO SDL.Texture
      drawDigit d =
        toTexture =<< Font.solid smallFont (V4 255 255 255 255) (showText d)
  num0 <- drawDigit 0
  num1 <- drawDigit 1
  num2 <- drawDigit 2
  num3 <- drawDigit 3
  num4 <- drawDigit 4
  num5 <- drawDigit 5
  num6 <- drawDigit 6
  num7 <- drawDigit 7
  num8 <- drawDigit 8
  num9 <- drawDigit 9
  let digitSprites = \n -> case n of
        Digit'0 -> num0
        Digit'1 -> num1
        Digit'2 -> num2
        Digit'3 -> num3
        Digit'4 -> num4
        Digit'5 -> num5
        Digit'6 -> num6
        Digit'7 -> num7
        Digit'8 -> num8
        Digit'9 -> num9
  --dinoSprites <-
  --  Animate.readSpriteSheetJSON loadTexture "resource/dino.json" :: IO
  --    (Animate.SpriteSheet DinoKey SDL.Texture Seconds)
  --birdSprites <-
  --  Animate.readSpriteSheetJSON loadTexture "resource/bird.json" :: IO
  --    (Animate.SpriteSheet BirdKey SDL.Texture Seconds)
  --lavaSprites <-
  --  Animate.readSpriteSheetJSON loadTexture "resource/lava.json" :: IO
  --    (Animate.SpriteSheet LavaKey SDL.Texture Seconds)
  --rockSprites <-
  --  Animate.readSpriteSheetJSON loadTexture "resource/rock.json" :: IO
  --    (Animate.SpriteSheet RockKey SDL.Texture Seconds)
  Font.free smallFont
  Font.free bigFont
  Font.free titleFont
  return Resources { rJungleSprites  = jungle
                   , rGroundSprites  = ground
                   , rGameMusic      = gameMusic
                   , rMuted          = muted
                   , rUnMuted        = unMuted
                   , rPauseSprite    = pauseSprite
                   , rSpaceSprite    = spaceSprite
                   , rEscapeSprite   = escapeSprite
                   , rGameOverSprite = gameOverSprite
                   , rHiscoreSprite  = hiscoreSprite
                   , rTitleSprite    = titleSprite
                   , rDigitSprites   = digitSprites
                   , rControlsSprite = controlsSprite
                   }
 where
  toTexture surface = SDL.createTextureFromSurface renderer surface
  loadTexture path c =
    SDL.createTextureFromSurface renderer =<< loadSurface path c

freeResources :: Resources -> IO ()
freeResources r = do
  --SDL.destroyTexture $ Animate.ssImage (rMountainSprites r)
  --SDL.destroyTexture (rJungleSprites r)
  --SDL.destroyTexture (rGroundSprites r)
  --SDL.destroyTexture $ Animate.ssImage (rRiverSprites r)
  --SDL.destroyTexture $ Animate.ssImage (rDinoSprites r)
  --SDL.destroyTexture $ Animate.ssImage (rBirdSprites r)
  --SDL.destroyTexture $ Animate.ssImage (rLavaSprites r)
  --SDL.destroyTexture $ Animate.ssImage (rRockSprites r)
  SDL.destroyTexture (rPauseSprite r)
  SDL.destroyTexture (rEscapeSprite r)
  SDL.destroyTexture (rSpaceSprite r)
  SDL.destroyTexture (rGameOverSprite r)
  SDL.destroyTexture (rHiscoreSprite r)
  SDL.destroyTexture (rTitleSprite r)
  SDL.destroyTexture (rControlsSprite r)

  Mixer.free (rGameMusic r)
  --Mixer.free (rJumpSfx r)
  --Mixer.free (rDuckSfx r)
  --Mixer.free (rPointSfx r)
  --Mixer.free (rBirdSfx r)
  --Mixer.free (rHurtSfx r)
  --Mixer.free (rLavaSfx r)
  --Mixer.free (rQuakeSfx r)
  --Mixer.free (rRockSfx r)
  --Mixer.free (rRecoverSfx r)
  --Mixer.free (rStockSfx r)
  --Mixer.free (rDeathSfx r)

data Digit
  = Digit'0
  | Digit'1
  | Digit'2
  | Digit'3
  | Digit'4
  | Digit'5
  | Digit'6
  | Digit'7
  | Digit'8
  | Digit'9
  deriving (Show, Eq, Enum, Bounded)

digitToInt :: Digit -> Int
digitToInt = fromEnum

toDigit :: Integer -> [Digit]
toDigit i = (if next == 0 then [] else toDigit next) ++ (single (i `mod` 10))
 where
  next = i `div` 10
  single 0 = [Digit'0]
  single 1 = [Digit'1]
  single 2 = [Digit'2]
  single 3 = [Digit'3]
  single 4 = [Digit'4]
  single 5 = [Digit'5]
  single 6 = [Digit'6]
  single 7 = [Digit'7]
  single 8 = [Digit'8]
  single 9 = [Digit'9]
  single _ = []

toDigitReverse :: Integer -> [Digit]
toDigitReverse = reverse . toDigit
