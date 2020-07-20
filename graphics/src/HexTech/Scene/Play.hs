module HexTech.Scene.Play where

import qualified Animate
import qualified Data.Map.Strict               as Map
import           Control.Monad                  ( when )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Vector.Storable          as Vector
import           Control.Lens            hiding ( zoom )
import           Control.Monad.State            ( MonadState(..)
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
import           HexTech.State                  ( HasSettings(..)
                                                , modifySettings
                                                , HasPlayVars(..)
                                                , PlayVars(..)
                                                , SceneManager(..)
                                                , SceneType(..)
                                                )
import qualified HexTech.Engine.Audio          as Audio
import           HexTech.Engine.Audio           ( Audio(..) )
import qualified HexTech.Engine.Types          as T
import           HexTech.Engine.Types           ( (<+>) )
import           HexTech.Engine.Types           ( Logger(..)
                                                , Clock(..)
                                                , Point(..)
                                                , frameDeltaSeconds
                                                , secondsToInteger
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
                                                , getSpriteAnimations
                                                , Color(..)
                                                , setColor
                                                , setColorV4
                                                , getColorV4
                                                )
import           HexTech.Camera                 ( CameraControl(..) )
import           HexTech.Wrapper.SDLRenderer    ( SDLRenderer(..) )
import qualified HexTech.Game                  as Game
import           HexTech.Game                   ( HasGame(..)
                                                , Piece(..)
                                                , Resource(..)
                                                , initPlayer
                                                , playerPieces
                                                , playerName
                                                , piecePosition
                                                )
import qualified HexTech.Grid                  as Grid
import           HexTech.Grid                   ( Grid(..)
                                                , Tile(..)
                                                , CubeCoord(..)
                                                , (+>)
                                                , directionShift
                                                , Direction(..)
                                                , gTiles
                                                )



playScene
  :: ( MonadState State.Model m
     , SceneManager m
     , Renderer m
     , CameraControl m
     , HasInput m
     , Audio m
     , Logger m
     , MonadReader Config m
     , Clock m
     )
  => State.Scene m
playScene = State.Scene { drawScene       = drawPlay
                        , stepScene       = playStep
                        , sceneTransition = playTransition
                        }

playTransition
  :: (HasPlayVars s, HasSettings s, MonadState s m, Audio m, Logger m) => m ()
playTransition = do
  let game_        = Game.twoPlayersGame
      activePlayer = case game_ ^? (gamePlayers . ix 0) of
        Just p  -> p
        Nothing -> Game.initPlayer "Empty" (CubeCoord (0, 0, 0))
  playVars .= State.initPlayVars activePlayer
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
        (  ksStatus (Input.iPause input)
        == KeyStatus'Pressed
        || ksStatus (Input.iEscape input)
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
  updatePlayerAction
  updateZoom
  updateCamera
  updateSettings

updatePlayerAction
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
updatePlayerAction = do
  grid  <- use (game . gameGrid)
  input <- getInput
  let enter = ksStatus (Input.iSelect input) == KeyStatus'Pressed
  mCurrentTile <- use (playVars . _pvSelectedTile)
  let selectedCoord = case mCurrentTile of
        Just (Tile { tileCoords }) -> tileCoords
        Nothing                    -> CubeCoord (0, 0, 0)
  mCurrentPlayer <- use (playVars . _pvActivePlayer)
  --logInfo input


  when enter $ do
    (game . gamePlayers . ix 0 . playerPieces . ix 0 . piecePosition)
      .= selectedCoord
    --v <- use (game . gamePlayers . ix 0 . playerPieces . ix 0 . piecePosition)
    --logInfo selectedCoord


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
  game_ <- use game
  tiles <- use (game . gameGrid . gTiles)
  grid  <- use (game . gameGrid)
  input <- getInput
  let up          = (ksStatus (Input.iUp input) == KeyStatus'Pressed)
      down        = (ksStatus (Input.iDown input) == KeyStatus'Pressed)
      left        = (ksStatus (Input.iLeft input) == KeyStatus'Pressed)
      right       = (ksStatus (Input.iRight input) == KeyStatus'Pressed)
      ctrl        = (ksStatus (Input.iCtrl input) == KeyStatus'Held)
      mouseClicks = Input.iMouseClick input
      selectTile mTile = case mTile of
        Just tile -> do
          let content = Game.onTile tile game_
          playVars . _pvTileContent .= Just content
          playVars . _pvSelectedTile .= Just tile
        Nothing -> return ()

  case mouseClicks of
    (mouseClick : _) -> do
      let rightBut = (mouseClick ^. Input.mouseButton == Input.ButtonLeft)
          mousePos = mouseClick ^. Input.mousePos
      when rightBut $ do
        --logInfo mouseClick
        --logInfo mousePos
        selectTile (Grid.pointToTile mousePos grid)

    [] -> return ()

  mCurrentTile <- use (playVars . _pvSelectedTile)

  let move dir = do
        case mCurrentTile of
          Just (Tile { tileCoords }) -> do
            let mTile = Map.lookup (tileCoords +> directionShift dir) tiles
            selectTile mTile
          Nothing ->
            (playVars . _pvSelectedTile)
              .= (Map.lookup (CubeCoord (0, 0, 0)) tiles)

  when up $ move (if ctrl then NorthWest else NorthEast)
  when down $ move (if ctrl then SouthEast else SouthWest)
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
  pv <- use playVars
  disableZoom
  grid <- use (game . gameGrid)
  drawGrid grid
  players       <- use (game . gamePlayers)
  mActivePlayer <- use (playVars . _pvActivePlayer)
  let tilePos pos =
        maybe (T.p 450 450) tileCenter $ Map.lookup pos (gridTiles grid)
  mapM_
    (\(i, player) -> do
      let
        name   = player ^. playerName
        active = maybe False
                       (\p -> player ^. playerName == p ^. playerName)
                       mActivePlayer
        text =
          T.showText (i + 1) <> ". " <> name <> (if active then " *" else "")
      drawText text (T.p 50 (100 + 28 * i))

      mapM_
        (\piece -> do
          let piecePoint = tilePos $ view piecePosition piece
          drawPiece piece piecePoint
        )
        (view playerPieces player)
    )
    (NonEmpty.zip (0 :| [1 ..]) players)

  resources <- use (game . freeResources)
  mapM_
    (\(pos, res) -> do
      let posPoint = tilePos pos
          text     = case res of
            Star -> "*"
            Plus -> "+"

      drawText text (posPoint <+> T.p (-12) (-10))
    )
    (Map.toList resources)

  drawControlsText (10, 710)
  drawDigits (secondsToInteger $ pvSeconds pv) (T.p 50 50)
  muted <- gets $ view (settings . sMuted)
  let muteSprite | muted     = (rMuted . cResources)
                 | otherwise = (rUnMuted . cResources)

  drawTextureSprite muteSprite (1000, 50)

  mContent <- use (playVars . _pvTileContent)
  case mContent of
    Just content -> drawTileContent content (T.p 1000 80)
    Nothing      -> return ()

  enableZoom


drawTileContent :: (Renderer m) => Game.TileContent -> Point -> m ()
drawTileContent content point = do
  drawText "Tile Contains:" point

  case content ^. Game.tileResource of
    Just res -> drawText ("Res: " <> T.showText res) (point <+> T.p 10 30)
    Nothing  -> return ()

  let pieces = content ^. Game.tilePieces

  mapM_
    (\(i, p) -> drawText (T.showText (p ^. Game.pieceType))
                         (point <+> T.p 10 (30 * i + 60))
    )
    (zip [0 ..] pieces)

drawPiece :: (Renderer m) => Piece -> Point -> m ()
drawPiece _piece point = do

  drawCommander Commander'Idle (T.p (-15) (-28) <+> point)


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
  mCurrentTile <- use (playVars . _pvSelectedTile)
  maybe (return ()) drawTile mCurrentTile

drawTile
  :: (HasPlayVars s, HasSettings s, MonadState s m, Renderer m) => Tile -> m ()
drawTile tile = do
  mCurrentTile <- use (playVars . _pvSelectedTile)
  let points                = (map T.toSDLPoint . tileCorners) tile
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
  :: ( MonadState s m
     , HasSettings s
     , HasPlayVars s
     , HasGame s
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
  let shallResume = (ksStatus (Input.iPause input) == KeyStatus'Pressed)
      shallQuit   = (ksStatus (Input.iEscape input) == KeyStatus'Pressed)
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
