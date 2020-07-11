module HexTech.Scene.Play where

import qualified Animate
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

import qualified HexTech.Input                 as Input
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
                                                , Point(..)
                                                , frameDeltaSeconds
                                                , secondsToInteger
                                                )
import           HexTech.Scene                  ( SceneManager(..)
                                                , SceneType(..)
                                                )
import           HexTech.Resource               ( Resources(..)
                                                , CommanderKey(..)
                                                )
import           HexTech.Engine.Renderer        ( Renderer(..)
                                                , drawBlackOverlay
                                                , drawTextureSprite
                                                , drawDigits
                                                , drawLines
                                                , drawSprite
                                                , drawText
                                                , toSDLPoint
                                                , getSpriteAnimations
                                                --, gridToPixels
                                                , Color(..)
                                                , setColor
                                                , setColorV4
                                                , getColorV4
                                                )
import           HexTech.Camera                 ( CameraControl(..) )
import           HexTech.Wrapper.SDLRenderer    ( SDLRenderer(..) )
import           HexTech.Game                   ( HasGame(..)
                                                , Player(..)
                                                , Piece(..)
                                                , ResourceType(..)
                                                , playerPieces
                                                , playerName
                                                , piecePosition
                                                )
import           HexTech.Grid                   ( Grid(..)
                                                , Tile(..)
                                                , CubeCoord(..)
                                                , (+>)
                                                , directionShift
                                                , Direction(..)
                                                , gTiles
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
  updateSelectedTile
  updateZoom
  updateCamera
  updateSettings
  --isDead <- getDead
  --when isDead (toScene Scene'Title)


updateSelectedTile
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
updateSelectedTile = do
  tiles <- use (game . gameGrid . gTiles)
  input <- getInput
  let up    = (ksStatus (Input.iUp input) == KeyStatus'Pressed)
      down  = (ksStatus (Input.iDown input) == KeyStatus'Pressed)
      left  = (ksStatus (Input.iLeft input) == KeyStatus'Pressed)
      right = (ksStatus (Input.iRight input) == KeyStatus'Pressed)

  mCurrentTile <- use (playVars . _pvSelectedTile)
  let move dir = do
        case mCurrentTile of
          Just (Tile { tileCoords }) -> do
            (playVars . _pvSelectedTile)
              .= (Map.lookup (tileCoords +> directionShift dir) tiles)
            logInfo mCurrentTile
          Nothing ->
            (playVars . _pvSelectedTile)
              .= (Map.lookup (CubeCoord (0, 0, 0)) tiles)
  when up $ move NorthEast
  when down $ move SouthWest
  when left $ move West
  when right $ move East

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
  pv <- gets (view playVars)
  disableZoom
  grid <- use (game . gameGrid)
  drawGrid grid
  players <- use (game . gamePlayers)
  let tilePos pos =
        maybe (T.p 450 450) tileCenter $ Map.lookup pos (gridTiles grid)
  mapM_
    (\(i, player) -> do
      let name = player ^. playerName
      drawText name (T.p 50 (100 + 28 * i))

      mapM_
        (\piece -> do
          let piecePoint = tilePos $ view piecePosition piece
          drawPiece piece piecePoint
        )
        (view playerPieces player)
    )
    (zip [0 ..] players)

  drawControlsText (10, 710)
  drawDigits (secondsToInteger $ pvSeconds pv) (T.p 50 50)
  muted <- gets $ view (settings . sMuted)
  let muteSprite | muted     = (rMuted . cResources)
                 | otherwise = (rUnMuted . cResources)

  drawTextureSprite muteSprite (1000, 50)
  enableZoom

drawPiece :: (Renderer m) => Piece -> Point -> m ()
drawPiece _piece point = do

  drawCommander Commander'Idle (T.p (-5) (-28) T.<+> point)


drawCommander :: Renderer m => CommanderKey -> Point -> m ()
drawCommander key (Point pos) = do
  animations <- getSpriteAnimations (rCommanderSprites . cResources)
  let aniLoc = Animate.currentLocation animations (Animate.initPosition key)
  drawSprite (rCommanderSprites . cResources) aniLoc pos


drawGrid
  :: (HasPlayVars s, HasSettings s, MonadState s m, Renderer m) => Grid -> m ()
drawGrid grid = do
  let tiles = gridTiles grid
  mapM_ drawTile $ Map.elems tiles

drawTile
  :: (HasPlayVars s, HasSettings s, MonadState s m, Renderer m) => Tile -> m ()
drawTile tile = do
  mCurrentTile <- use (playVars . _pvSelectedTile)
  let points                = (map toSDLPoint . tileCorners) tile
      p                     = tileCenter tile
      (CubeCoord (x, y, z)) = tileCoords tile
  oldColor <- getColorV4
  let color | maybe False (tile ==) mCurrentTile = Red
            | otherwise                          = Green
  setColor color
  drawLines $ Vector.fromList points
  setColorV4 oldColor

  showCoords <- gets $ view (settings . sShowCoords)
  when showCoords $ do
    drawDigits (fromIntegral x) (T.p (-25) (-28) T.<+> p)
    drawDigits (fromIntegral y) (T.p (-5) (5) T.<+> p)
    drawDigits (fromIntegral z) (T.p (25) (-28) T.<+> p)



modifyPlayVars
  :: (MonadState s m, HasPlayVars s) => (PlayVars -> PlayVars) -> m ()
modifyPlayVars f = playVars %= f

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

pauseToPlay :: Audio m => m ()
pauseToPlay = raiseGameMusic

playToPause :: Audio m => m ()
playToPause = lowerGameMusic

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
