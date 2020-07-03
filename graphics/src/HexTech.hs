{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HexTech
  ( main
  )
where

import           Control.Monad                  ( foldM )
import           Control.Monad.Extra            ( whileM )

import           HexTech.Graphics               ( makeWindow )
import           HexTech.Game
import           HexTech.Engine.Vars
import           HexTech.Config
import           HexTech.State
import           Control.Monad.State
import           Control.Monad.Reader
import qualified SDL
import qualified SDL.Mixer                     as Mixer
import qualified SDL.Font                      as Font
import qualified Data.Text.IO                  as T



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
    SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  renderer  <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  --mkObstacles <- streamOfObstacles <$> getStdGen
  let
    cfg =
      Config { cWindow = window, cRenderer = renderer, cResources = resources }
  runHexTech cfg (initVars mkObstacles) mainLoop
  SDL.destroyWindow window
  freeResources resources
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  SDL.quit


mainLoop
  :: ( MonadReader Config m
     , MonadState Vars m
     , Audio m
     , AudioSfx m
     , Logger m
     , Clock m
     , CameraControl m
     , Renderer m
     , HasInput m
     , Title m
     , Play m
     , Pause m
     , Death m
     , GameOver m
     )
  => m ()
mainLoop = do
  updateInput
  input <- getInput
  clearScreen
  clearSfx
  scene <- gets vScene
  updateQuake
  step scene
  playSfx
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
      Scene'Title    -> titleStep
      Scene'Play     -> playStep
      Scene'Pause    -> pauseStep
      Scene'Death    -> deathStep
      Scene'GameOver -> gameOverStep
      Scene'Quit     -> return ()

  stepScene scene nextScene = do
    when (nextScene /= scene) $ do
      case nextScene of
        Scene'Title -> titleTransition
        Scene'Play  -> case scene of
          Scene'Title -> playTransition
          Scene'Pause -> pauseToPlay
          _           -> return ()
        Scene'Death -> case scene of
          Scene'Play -> deathTransition
          _          -> return ()
        Scene'Pause -> case scene of
          Scene'Play -> playToPause
          _          -> return ()
        Scene'GameOver -> return ()
        Scene'Quit     -> return ()
      modify (\v -> v { vScene = nextScene })
