{-# LANGUAGE TemplateHaskell #-}
module HexTech.State where

import qualified SDL
import           SDL                            ( ($=) )
import           Linear
import           Control.Lens
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Control.Monad.State            ( MonadState
                                                , modify
                                                , gets
                                                )

import           HexTech.Engine.Audio           ( Audio )
import           HexTech.Engine.Types           ( Logger )
import           HexTech.Engine.Renderer        ( Renderer )
import qualified HexTech.Game                  as Game
import           HexTech.Grid                   ( Tile(..)
                                                , CubeCoord(..)
                                                )

import           HexTech.Config                 ( Config(..)
                                                , screenHeight
                                                , screenWidth
                                                , screenV2
                                                )
import           HexTech.Input                  ( Input(..)
                                                , HasInput(..)
                                                , initInput
                                                )
import           HexTech.Engine.Types           ( Seconds )



data TitleVars = TitleVars
  { tvFlashing :: Float
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars 0

data PlayVars = PlayVars
  { pvSeconds :: Seconds
  , pvZoom :: Float
  , pvSelectedTile :: Maybe Tile
  , pvTileContent :: Maybe Game.TileContent
  , pvActivePlayer :: Maybe Game.Player
  , pvPlayerActions :: [Game.Action]
  } deriving (Show, Eq)

makeClassy_ ''PlayVars

initPlayVars :: Game.Player -> PlayVars
initPlayVars activePlayer = PlayVars { pvSeconds       = 0
                                     , pvZoom          = 1
                                     , pvSelectedTile  = Nothing
                                     , pvTileContent   = Nothing
                                     , pvActivePlayer  = Just activePlayer
                                     , pvPlayerActions = []
                                     }

data Settings = Settings
    { _sMuted :: Bool
    , _sShowCoords :: Bool
    } deriving (Show, Eq)
makeClassy ''Settings

initSettings :: Settings
initSettings = Settings { _sMuted = False, _sShowCoords = False }

modifySettings
  :: (MonadState s m, HasSettings s) => (Settings -> Settings) -> m ()
modifySettings f = modify $ settings %~ f

data SceneType
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)

data Camera = Camera
  { camOrigin :: V2 Float
  , camZoom :: V2 Float
  } deriving (Show, Eq)

makeClassy ''Camera

moveCamera :: MonadIO m => SDL.Renderer -> Camera -> m ()
moveCamera renderer Camera { camZoom, camOrigin } = do
  SDL.rendererScale renderer $= (fmap realToFrac camZoom)
  let dim = fmap truncate $ screenV2
  SDL.rendererViewport renderer
    $= ( Just
       $ SDL.Rectangle (SDL.P $ (fmap truncate $ moveOrigin camOrigin)) dim
       )
  SDL.rendererClipRect renderer $= (Just $ SDL.Rectangle (SDL.P $ V2 0 0) dim)


class Monad m => CameraControl m where
  adjustCamera :: Camera -> m ()
  disableZoom :: m ()
  enableZoom :: m ()

initCamera :: Camera
initCamera = Camera (V2 (screenWidth / 2) (screenHeight / 2)) (V2 1 1)

moveOrigin :: V2 Float -> V2 Float
moveOrigin (V2 x y) = V2 (screenWidth / 2 - x) (screenHeight / 2 - y)

lerpCamera :: Float -> Camera -> Camera -> Camera
lerpCamera p a b =
  Camera (lerp p (camOrigin a) (camOrigin b)) (lerp p (camZoom a) (camZoom b))

-- State of the program
data Model = Model
  { vGame :: Game.Game
  , vScene :: SceneType
  , vNextScene :: SceneType
  , vTitle :: TitleVars
  , vPlay :: PlayVars
  , vInput :: Input
  , vCamera :: Camera
  , vSettings :: Settings
  } deriving (Show, Eq)
makeClassy_ ''Model

initModel :: Model
initModel = Model { vGame      = game
                  , vScene     = Scene'Title
                  , vNextScene = Scene'Title
                  , vTitle     = initTitleVars
                -- TODO dont init until game start
                  , vPlay      = initPlayVars activePlayer
                  , vInput     = initInput
                  , vCamera    = initCamera
                  , vSettings  = initSettings
                  }
 where
  game         = Game.twoPlayersGame
  activePlayer = case game ^? (Game.gamePlayers . ix 0) of
    Just p  -> p
    Nothing -> Game.initPlayer "Empty" (CubeCoord (0, 0, 0))


data Scene :: (* -> *) -> * where
    Scene ::( MonadState Model m
            , SceneManager m
            , Renderer m
            , CameraControl m
            , HasInput m
            , Audio m
            , Logger m
            , MonadReader Config m
            ) =>
        { drawScene :: m ()
        , stepScene :: m ()
        , sceneTransition :: m ()
        } -> Scene m


class Monad m => SceneManager m where
  toScene :: SceneType -> m ()


toScene' :: MonadState Model m => SceneType -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

instance Game.HasGame Model where
  game = lens vGame (\v s -> v { vGame = s })

instance HasSettings Model where
  settings = lens vSettings (\v s -> v { vSettings = s })

--instance HasCommonVars Model where
--  commonVars = lens vCommon (\v s -> v { vCommon = s })
--
instance HasTitleVars Model where
  titleVars = lens vTitle (\v s -> v { vTitle = s })

instance HasPlayVars Model where
  playVars = lens vPlay (\v s -> v { vPlay = s })
--
--instance HasGameOverVars Model where
--  gameOverVars = lens vGameOver (\v s -> v { vGameOver = s })

adjustCamera'
  :: (MonadIO m, MonadReader Config m, MonadState Model m) => Camera -> m ()
adjustCamera' cam = do
  modify $ \v -> set _vCamera cam v
  renderer <- asks cRenderer
  moveCamera renderer cam

disableZoom' :: (MonadIO m, MonadReader Config m) => m ()
disableZoom' = do
  renderer <- asks cRenderer
  moveCamera renderer initCamera

enableZoom' :: (MonadIO m, MonadReader Config m, MonadState Model m) => m ()
enableZoom' = do
  renderer <- asks cRenderer
  cam      <- gets vCamera
  moveCamera renderer cam

getInput' :: MonadState Model m => m Input
getInput' = gets vInput

setInput' :: MonadState Model m => Input -> m ()
setInput' input = modify (\v -> v { vInput = input })
