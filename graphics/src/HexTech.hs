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
import qualified Data.Text.IO                  as T


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
import           HexTech.Scene.Title            ( HasTitleVars
                                                , initTitleVars
                                                , titleVars
                                                , titleStep
                                                )
import           HexTech.Scene.Play             ( playTransition
                                                , playStep
                                                , pauseStep
                                                )
import           HexTech.Resource               ( loadResources
                                                , Resources(..)
                                                , freeResources
                                                )
import           HexTech.Input                  ( HasInput
                                                --, iEscape
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
    SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1280 920 }
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
  let quit = nextScene == Scene'Quit || iQuit input
  unless quit mainLoop
 where

  step scene = do
    case scene of
      Scene'Title    -> titleStep
      Scene'Play     -> playStep
      Scene'Pause    -> pauseStep
      Scene'GameOver -> return ()
      Scene'Quit     -> return ()

  stepScene scene nextScene = do
    when (nextScene /= scene) $ do
      case nextScene of
        Scene'Title -> titleTransition
        Scene'Play  -> case scene of
          Scene'Title -> playTransition
          Scene'Pause -> pauseToPlay
          _           -> return ()
        Scene'Pause -> case scene of
          Scene'Play -> playToPause
          _          -> return ()
        Scene'GameOver -> return ()
        Scene'Quit     -> return ()
      modify (\v -> v { vScene = nextScene })

titleTransition
  :: (HasTitleVars a, MonadState a m, CameraControl m, Audio m) => m ()
titleTransition = do
  stopGameMusic
  adjustCamera initCamera
  modify $ titleVars .~ initTitleVars

pauseToPlay :: Audio m => m ()
pauseToPlay = raiseGameMusic

playToPause :: Audio m => m ()
playToPause = lowerGameMusic

instance Audio HexTech where
  playGameMusic  = playGameMusic'
  stopGameMusic  = stopGameMusic'
  lowerGameMusic = lowerGameMusic'
  raiseGameMusic = raiseGameMusic'

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
  drawGameOverText    = drawTextureSprite (rGameOverSprite . cResources)
  drawPressSpaceText  = drawTextureSprite (rSpaceSprite . cResources)
  drawPressEscapeText = drawTextureSprite (rEscapeSprite . cResources)
  drawTitleText       = drawTextureSprite (rTitleSprite . cResources)
  drawControlsText    = drawTextureSprite (rControlsSprite . cResources)
