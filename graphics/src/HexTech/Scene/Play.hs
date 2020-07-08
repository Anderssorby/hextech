module HexTech.Scene.Play where

--import qualified Animate
import qualified Data.Map.Strict               as Map
import           Control.Monad                  ( when
                                                --, foldM
                                                --, mapM_
                                                )
import qualified Data.Vector.Storable          as Vector
import           Control.Lens            hiding ( zoom )
import           Control.Monad.State            ( MonadState(..)
                                                , modify
                                                , gets
                                                )
import           Control.Monad.Reader           ( MonadReader )
import           KeyState

import           HexTech.Input                  ( HasInput(..)
                                                , Input(..)
                                                )
import           HexTech.Config                 ( Config(..) )
import qualified HexTech.State                 as State
import           HexTech.State                  ( Vars(..)
                                                , HasSettings(..)
                                                , modifySettings
                                                , HasPlayVars(..)
                                                , PlayVars(..)
                                                )
import qualified HexTech.Engine.Audio          as Audio
import           HexTech.Engine.Audio           ( Audio(..) )
import qualified HexTech.Engine.Types          as T
import           HexTech.Engine.Types           ( Logger(..)
                                                , Clock(..)
                                                , frameDeltaSeconds
                                                , secondsToInteger
                                                )
import           HexTech.Scene                  ( SceneManager(..)
                                                , Scene(..)
                                                )
import           HexTech.Resource               ( Resources(..)
                                                , CommanderKey(..)
                                                )
import           HexTech.Engine.Renderer        ( Renderer(..)
                                                , drawBlackOverlay
                                                , drawCommander
                                                , drawTextureSprite
                                                , drawDigits
                                                , drawLines
                                                , toSDLPoint
                                                --, gridToPixels
                                                , Color(..)
                                                , setColor
                                                , setColorV4
                                                , getColorV4
                                                )
import           HexTech.Camera                 ( CameraControl(..) )
import           HexTech.Wrapper.SDLRenderer    ( SDLRenderer(..) )
import           HexTech.Game                   ( HasGame(..) )
import           HexTech.Grid                   ( Grid(..)
                                                , Tile(..)
                                                , CubeCoord(..)
                                                )




playTransition
  :: (HasPlayVars s, HasSettings s, MonadState s m, Audio m, Logger m) => m ()
playTransition = do
  playVars .= State.initPlayVars
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
  let shallPause =
        (  ksStatus (iSpace input)
        == KeyStatus'Pressed
        || ksStatus (iEscape input)
        == KeyStatus'Pressed
        )
  when shallPause (toScene Scene'Pause)
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
  updateSettings
  --isDead <- getDead
  --when isDead (toScene Scene'Title)

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
  drawGrid grid
  drawControlsText (50, 470)
  drawDigits (secondsToInteger $ pvSeconds pv) (T.p 50 50)
  muted <- gets $ view (settings . sMuted)
  let muteSprite | muted     = (rMuted . cResources)
                 | otherwise = (rUnMuted . cResources)

  drawTextureSprite muteSprite (1000, 50)
  enableZoom

drawGrid :: (HasSettings s, MonadState s m, Renderer m) => Grid -> m ()
drawGrid grid = do
  let tiles = gridTiles grid
  mapM_ drawTile $ Map.elems tiles
  drawCommander Commander'Idle (300, 300)

drawTile :: (HasSettings s, MonadState s m, Renderer m) => Tile -> m ()
drawTile tile = do
  let points                = (map toSDLPoint . tileCorners) tile
      p                     = tileCenter tile
      (CubeCoord (x, y, z)) = tileCoords tile
  oldColor <- getColorV4
  setColor Green
  drawLines $ Vector.fromList points
  setColorV4 oldColor

  showCoords <- gets $ view (settings . sShowCoords)
  when showCoords $ do
    drawDigits (fromIntegral x) (T.p (-25) (-10) T.<+> p)
    drawDigits (fromIntegral y) (T.p (0) (-10) T.<+> p)
    drawDigits (fromIntegral z) (T.p (25) (-10) T.<+> p)



modifyPlayVars
  :: (MonadState s m, HasPlayVars s) => (PlayVars -> PlayVars) -> m ()
modifyPlayVars f = modify $ playVars %~ f

updateSeconds :: (MonadState s m, HasPlayVars s) => m ()
updateSeconds =
  playVars %= \pv -> pv { pvSeconds = pvSeconds pv + frameDeltaSeconds }


updateZoom :: (MonadState s m, HasPlayVars s) => m ()
updateZoom = playVars %= \pv -> pv { pvZoom = pvZoom pv }

updateCamera :: (MonadState s m, HasPlayVars s, CameraControl m) => m ()
updateCamera = do
  --zoom <- gets (pvZoom . view playVars)
  --let cam = lerpCamera ((1 - zoom) ** (1.8 :: Float)) duckCamera initCamera
  --adjustCamera cam
  return ()


getDead :: (MonadState s m, HasPlayVars s) => m Bool
getDead = (>= 100) <$> gets (pvSeconds . view playVars)

updateSettings
  :: (HasSettings s, MonadState s m, HasInput m, Logger m, Audio m) => m ()
updateSettings = do
  updateMuted
  updateShowCoords

updateMuted
  :: (HasSettings s, MonadState s m, HasInput m, Logger m, Audio m) => m ()
updateMuted = do
  input <- getInput
  when (ksStatus (iMute input) == KeyStatus'Pressed) $ do
    modifySettings (\s -> sMuted .~ (not $ s ^. sMuted) $ s)

updateShowCoords
  :: (HasSettings s, MonadState s m, HasInput m, Logger m, Audio m) => m ()
updateShowCoords = do
  input <- getInput
  when (ksStatus (iShowCoords input) == KeyStatus'Pressed) $ do
    modifySettings (\s -> sShowCoords .~ (not $ s ^. sShowCoords) $ s)

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
  updateSettings
  updateGameMusic
  let shallResume = (ksStatus (iSpace input) == KeyStatus'Pressed)
      shallQuit   = (ksStatus (iEscape input) == KeyStatus'Pressed)
  when shallResume (toScene Scene'Play)
  when shallQuit   (toScene Scene'Title)

drawPauseText
  :: (SDLRenderer m, Renderer m, MonadReader Config m) => (Int, Int) -> m ()
drawPauseText = drawTextureSprite (rPauseSprite . cResources)

drawPause :: (Renderer m, CameraControl m, MonadState s m, HasGame s) => m ()
drawPause = do
  drawBlackOverlay 0.8
  disableZoom
  drawPauseText (530, 270)
  enableZoom
