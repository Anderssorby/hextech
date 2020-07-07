module HexTech.Scene.Play where

--import qualified Animate
import           Control.Monad                  ( when
                                                --, mapM_
                                                )
import           Control.Lens            hiding ( zoom )
import           Control.Monad.State            ( MonadState(..)
                                                , modify
                                                , gets
                                                )
import           Control.Monad.Reader           ( MonadReader )
--import qualified Data.Text.IO                  as T
import           KeyState

import           HexTech.Input                  ( HasInput(..)
                                                , Input(..)
                                                )
import           HexTech.Config                 ( Config(..) )
import           HexTech.State                  ( Vars(..)
                                                , HasSettings(..)
                                                , modifySettings
                                                , HasPlayVars(..)
                                                , PlayVars(..)
                                                )
import qualified HexTech.Engine.Audio          as Audio
import           HexTech.Engine.Audio           ( Audio(..) )
import           HexTech.Engine.Types           ( Logger(..)
                                                , Clock(..)
                                                , frameDeltaSeconds
                                                , secondsToInteger
                                                )
import           HexTech.Scene                  ( SceneManager(..)
                                                , Scene(..)
                                                )
import           HexTech.Resource               ( Resources(..) )
import           HexTech.Engine.Renderer        ( Renderer(..)
                                                , drawBlackOverlay
                                                , drawTextureSprite
                                                , drawDigits
                                                , drawLines
                                                , gridToPixels
                                                , Color(..)
                                                , setColor
                                                , setColorV4
                                                , getColorV4
                                                )
import           HexTech.Camera                 ( CameraControl(..) )
import           HexTech.Wrapper.SDLRenderer    ( SDLRenderer(..) )
import           HexTech.Game                   ( HasGame(..) )
import           HexTech.Grid                   ( Grid )




playTransition
  :: (HasPlayVars s, HasSettings s, MonadState s m, Audio m, Logger m) => m ()
playTransition = do
  --PlayVars { pvUpcomingObstacles } <- gets (view playVars)
  --modify $ playVars .~ (initPlayVars pvUpcomingObstacles)
  playGameMusic

updateGameMusic :: (HasSettings s, MonadState s m, Audio m, Logger m) => m ()
updateGameMusic = do
  muted <- gets $ view (settings . sMuted)
  Audio.toggleMusic (not muted)

playStep
  :: ( HasPlayVars s
     , HasGame s
     , HasSettings s
     , MonadState s m
     , Logger m
     , CameraControl m
     , Clock m
     , Renderer m
     , SDLRenderer m
     , Audio m
     --, AudioSfx m
     , HasInput m
     , SceneManager m
     , MonadReader Config m
     )
  => m ()
playStep = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Pause)
  updatePlay
  updateGameMusic
  drawPlay

updatePlay
  :: ( HasPlayVars s
     , HasSettings s
     , HasGame s
     , MonadState s m
     , Logger m
     , Clock m
     , CameraControl m
     , Renderer m
     , HasInput m
     , Audio m
     --, AudioSfx m
     , SceneManager m
     )
  => m ()
updatePlay = do
  updateSeconds
  updateZoom
  updateCamera
  updateMuted
  isDead <- getDead
  when isDead (toScene Scene'Title)

{-| Draw the game play view
-}
drawPlay
  :: ( HasPlayVars s
     , HasGame s
     , HasSettings s
     , MonadState s m
     , Renderer m
     , CameraControl m
     )
  => m ()
drawPlay = do
  pv   <- gets (view playVars)
  grid <- gets $ view (game . gameGrid)
  disableZoom
  drawGrid grid (450, 450)
  drawControlsText (50, 470)
  drawDigits (secondsToInteger $ pvSeconds pv) (50, 50)
  muted <- gets $ view (settings . sMuted)
  let muteSprite | muted     = (rMuted . cResources)
                 | otherwise = (rUnMuted . cResources)

  drawTextureSprite muteSprite (900, 50)
  enableZoom

drawGrid :: (Renderer m, MonadReader Config m) => Grid -> (Int, Int) -> m ()
drawGrid grid point = do
  let points = gridToPixels grid point
  oldColor <- getColorV4
  setColor Green
  mapM_ drawLines points
  setColorV4 oldColor


modifyPlayVars
  :: (MonadState s m, HasPlayVars s) => (PlayVars -> PlayVars) -> m ()
modifyPlayVars f = modify $ playVars %~ f

updateSeconds :: (MonadState s m, HasPlayVars s) => m ()
updateSeconds =
  modifyPlayVars $ \pv -> pv { pvSeconds = pvSeconds pv + frameDeltaSeconds }


updateZoom :: (MonadState s m, HasPlayVars s) => m ()
updateZoom = modifyPlayVars $ \pv -> pv { pvZoom = pvZoom pv }

updateCamera :: (MonadState s m, HasPlayVars s, CameraControl m) => m ()
updateCamera = do
  --zoom <- gets (pvZoom . view playVars)
  --let cam = lerpCamera ((1 - zoom) ** (1.8 :: Float)) duckCamera initCamera
  --adjustCamera cam
  return ()


getDead :: (MonadState s m, HasPlayVars s) => m Bool
getDead = (>= 100) <$> gets (pvSeconds . view playVars)

updateMuted
  :: (HasSettings s, MonadState s m, HasInput m, Logger m, Audio m) => m ()
updateMuted = do
  input <- getInput
  when (ksStatus (iMute input) == KeyStatus'Pressed) $ do
    --logShow input
    --sett <- gets (view settings)
    --logShow sett
    modifySettings (\s -> sMuted .~ (not $ s ^. sMuted) $ s)

pauseStep
  :: ( MonadState Vars m
     , SceneManager m
     , HasInput m
     , Renderer m
     , CameraControl m
     , Logger m
     , Audio m
     )
  => m ()
pauseStep = do
  input <- getInput
  drawPlay
  drawPause
  updateMuted
  updateGameMusic
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)

drawPauseText
  :: (SDLRenderer m, Renderer m, MonadReader Config m) => (Int, Int) -> m ()
drawPauseText = drawTextureSprite (rPauseSprite . cResources)

drawPause :: (Renderer m, CameraControl m, MonadState s m, HasGame s) => m ()
drawPause = do
  drawBlackOverlay 0.8
  disableZoom
  drawPauseText (530, 270)
  enableZoom
