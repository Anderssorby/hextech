module HexTech
  ( main
  )
where

import           Control.Lens
import           Control.Exception.Safe         ( MonadThrow
                                                , MonadCatch
                                                )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.State
import           Control.Monad.Reader
import qualified SDL
import qualified SDL.Mixer                     as Mixer
import qualified SDL.Font                      as Font
import           Data.Text
import qualified Data.Text.IO                  as T
import           KeyState


import           HexTech.Graphics               ( makeWindow )
import           HexTech.Game                   ( Game(..) )
import           HexTech.Engine.Types           ( Clock(..)
                                                , Logger(..)
                                                , frameDeltaMilliseconds
                                                )
import           HexTech.Engine.Renderer        ( Renderer(..)
                                                , drawTextureSprite
                                                , clearScreen'
                                                , drawScreen'
                                                )
import           HexTech.Engine.Audio           ( Audio(..)
                                                , playGameMusic'
                                                , stopGameMusic'
                                                , lowerGameMusic'
                                                , raiseGameMusic'
                                                )
import           HexTech.Camera                 ( CameraControl(..)
                                                , initCamera
                                                )
import           HexTech.Config                 ( Config(..) )
import           HexTech.State                  ( Vars(..)
                                                , initVars
                                                , enableZoom'
                                                , adjustCamera'
                                                , disableZoom'
                                                , getInput'
                                                , setInput'
                                                , toScene'
                                                )
import           HexTech.Scene                  ( Scene(..)
                                                , SceneManager(..)
                                                )
import           HexTech.Scene.Title            ( Title(..)
                                                , HasTitleVars
                                                , initTitleVars
                                                , titleVars
                                                , titleStep'
                                                )
import           HexTech.Scene.Play             ( HasPlayVars
                                                , initPlayVars
                                                , playVars
                                                , playStep
                                                , pauseStep
                                                )
import           HexTech.Resource               ( loadResources
                                                , Resources(..)
                                                , freeResources
                                                )
import           HexTech.Input                  ( HasInput
                                                , iEscape
                                                , iQuit
                                                , getInput
                                                , setInput
                                                , updateInput
                                                , updateInput'
                                                )
import           HexTech.Wrapper.SDLInput       ( SDLInput(..)
                                                , pollEventPayloads'
                                                )
import           HexTech.Wrapper.SDLRenderer    ( SDLRenderer(..)
                                                , drawTexture'
                                                , presentRenderer'
                                                , clearRenderer'
                                                , queryTexture'
                                                )



newtype HexTech a = HexTech (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO, MonadThrow, MonadCatch)

runHexTech :: Config -> Vars -> HexTech a -> IO a
runHexTech config v (HexTech m) = evalStateT (runReaderT m config) v

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
  window <- SDL.createWindow
    "Hex Tech"
    SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1280 720 }
  renderer  <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  --mkObstacles <- streamOfObstacles <$> getStdGen
  let
    cfg =
      Config { cWindow = window, cRenderer = renderer, cResources = resources }
  runHexTech cfg initVars mainLoop
  SDL.destroyWindow window
  freeResources resources
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  SDL.quit


{-| The main loop of our GUI program handling State, Logging, Input, Rendering and so on.
-}
mainLoop
  :: ( MonadReader Config m
     , MonadState Vars m
     , Audio m
     --, AudioSfx m
     , Logger m
     , Clock m
     , CameraControl m
     , Renderer m
     , SDLRenderer m
     , MonadIO m
     , HasInput m
     , Title m
     , SceneManager m
     )
  => m ()
mainLoop = do
  updateInput
  input <- getInput
  clearScreen
  --clearSfx
  scene <- gets vScene
  --updateQuake
  step scene
  --playSfx
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  nextScene <- gets vNextScene
  stepScene scene nextScene
  let quit =
        nextScene
          == Scene'Quit
          || iQuit input
          || ksStatus (iEscape input)
          == KeyStatus'Pressed
  unless quit mainLoop
 where

  step scene = do
    case scene of
      Scene'Title -> titleStep
      Scene'Play  -> playStep
      Scene'Pause -> pauseStep
      --Scene'Death    -> deathStep
      --Scene'GameOver -> gameOverStep
      Scene'Quit  -> return ()
      _           -> return ()

  stepScene scene nextScene = do
    when (nextScene /= scene) $ do
      case nextScene of
        Scene'Title -> titleTransition
        Scene'Play  -> case scene of
          Scene'Title -> playTransition
          Scene'Pause -> pauseToPlay
          _           -> return ()
        --Scene'Death -> case scene of
        --  Scene'Play -> deathTransition
        --  _          -> return ()
        Scene'Pause -> case scene of
          Scene'Play -> playToPause
          _          -> return ()
        Scene'GameOver -> return ()
        Scene'Quit     -> return ()
      modify (\v -> v { vScene = nextScene })

titleTransition :: (HasTitleVars a, MonadState a m, CameraControl m) => m ()
titleTransition = do
  adjustCamera initCamera
  modify $ titleVars .~ initTitleVars


playTransition :: (HasPlayVars a, MonadState a m, Audio m) => m ()
playTransition = do
  --PlayVars { pvUpcomingObstacles } <- gets (view playVars)
  --modify $ playVars .~ (initPlayVars pvUpcomingObstacles)
  playGameMusic

--deathTransition :: (Audio m) => m ()
--deathTransition = do
--  stopGameMusic
--  playDeathSfx

pauseToPlay :: Audio m => m ()
pauseToPlay = raiseGameMusic

playToPause :: Audio m => m ()
playToPause = lowerGameMusic

instance Audio HexTech where
  playGameMusic  = playGameMusic'
  stopGameMusic  = stopGameMusic'
--  playJumpSfx    = playJumpSfx'
--  playDuckSfx    = playDuckSfx'
--  playPointSfx   = playPointSfx'
--  playBirdSfx    = playBirdSfx'
--  playHurtSfx    = playHurtSfx'
--  playLavaSfx    = playLavaSfx'
--  playQuakeSfx   = playQuakeSfx'
--  playRockSfx    = playRockSfx'
--  playRecoverSfx = playRecoverSfx'
--  playDeathSfx   = playDeathSfx'
  lowerGameMusic = lowerGameMusic'
  raiseGameMusic = raiseGameMusic'
--  playStockSfx   = playStockSfx'
--

delayMilliseconds' :: Int -> IO ()
delayMilliseconds' ms = threadDelay (1000 * ms)

instance HasInput HexTech where
  updateInput = updateInput'
  getInput    = getInput'
  setInput    = setInput'

instance SceneManager HexTech where
  toScene = toScene'

instance Clock HexTech where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger HexTech where
  logText = liftIO . T.putStrLn

instance CameraControl HexTech where
  adjustCamera = adjustCamera'
  disableZoom  = disableZoom'
  enableZoom   = enableZoom'

instance SDLRenderer HexTech where
  drawTexture     = drawTexture'
  presentRenderer = presentRenderer'
  clearRenderer   = clearRenderer'
  queryTexture    = queryTexture'

instance SDLInput HexTech where
  pollEventPayloads = pollEventPayloads'

instance Renderer HexTech where
  clearScreen         = clearScreen'
  drawScreen          = drawScreen'
  --getDinoAnimations     = getSpriteAnimations (rDinoSprites . cResources)
  --getLavaAnimations     = getSpriteAnimations (rLavaSprites . cResources)
  --getRockAnimations     = getSpriteAnimations (rRockSprites . cResources)
  --getBirdAnimations     = getSpriteAnimations (rBirdSprites . cResources)
  --getMountainAnimations = getSpriteAnimations (rMountainSprites . cResources)
  --getRiverAnimations    = getSpriteAnimations (rRiverSprites . cResources)
  --drawDino              = drawSprite (rDinoSprites . cResources)
  --drawLava              = drawSprite (rLavaSprites . cResources)
  --drawRock              = drawSprite (rRockSprites . cResources)
  --drawBird              = drawSprite (rBirdSprites . cResources)
  --drawMountain = drawHorizontalScrollSprite (rMountainSprites . cResources) 16
  --drawJungle = drawHorizontalScrollImage (rJungleSprites . cResources) 8
  --drawGround = drawHorizontalScrollImage (rGroundSprites . cResources) 2
  --drawRiver = drawHorizontalScrollSprite (rRiverSprites . cResources) 4
  --drawBlackOverlay      = drawBlackOverlay'
  --drawHiscoreText       = drawTextureSprite (rHiscoreSprite . cResources)
  --drawPauseText       = drawTextureSprite (rPauseSprite . cResources)
  drawGameOverText    = drawTextureSprite (rGameOverSprite . cResources)
  drawPressSpaceText  = drawTextureSprite (rSpaceSprite . cResources)
  drawPressEscapeText = drawTextureSprite (rEscapeSprite . cResources)
  drawTitleText       = drawTextureSprite (rTitleSprite . cResources)
  --drawNumber n = drawTextureSprite (flip rNumberSprites n . cResources)
  drawControlsText    = drawTextureSprite (rControlsSprite . cResources)


instance Title HexTech where
  titleStep = titleStep'
