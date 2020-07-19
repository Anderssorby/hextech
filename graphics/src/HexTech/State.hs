{-# LANGUAGE TemplateHaskell #-}
module HexTech.State where

import           Control.Lens
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Control.Monad.State            ( MonadState
                                                , modify
                                                , gets
                                                )

import qualified HexTech.Game                  as Game
import           HexTech.Game                   ( Game
                                                , TileContent(..)
                                                , HasGame(..)
                                                , Player(..)
                                                , initPlayer
                                                , twoPlayersGame
                                                )
import           HexTech.Grid                   ( Tile(..)
                                                , CubeCoord(..)
                                                )

import           HexTech.Config                 ( Config(..) )
import           HexTech.Scene                  ( SceneType(..) )
import           HexTech.Scene.Title            ( TitleVars
                                                , HasTitleVars(..)
                                                , initTitleVars
                                                )
import           HexTech.Camera                 ( Camera(..)
                                                , initCamera
                                                , moveCamera
                                                )
import           HexTech.Input                  ( Input(..)
                                                , initInput
                                                )
import           HexTech.Engine.Types           ( Seconds )


data PlayVars = PlayVars
  {
  --pvScore :: Score
  --, pvStocks :: Stocks
  --, pvSpeed :: Percent
    pvSeconds :: Seconds
  , pvZoom :: Float
  , pvSelectedTile :: Maybe Tile
  , pvTileContent :: Maybe TileContent
  , pvActivePlayer :: Maybe Player
  , pvPlayerActions :: [Game.Action]
  --, pvShowDino :: Bool
  --, pvDinoPos :: Animate.Position DinoKey Seconds
  --, pvMountainPos :: Animate.Position MountainKey Seconds
  --, pvRiverPos :: Animate.Position RiverKey Seconds
  --, pvDinoState :: DinoState
  --, pvMountainScroll :: Distance
  --, pvJungleScroll :: Distance
  --, pvGroundScroll :: Distance
  --, pvRiverScroll :: Distance
  --, pvObstacles :: [ObstacleState]
  --, pvUpcomingObstacles :: [(Int, ObstacleTag)]
  } deriving (Show, Eq)

makeClassy_ ''PlayVars

initPlayVars :: Player -> PlayVars
initPlayVars activePlayer = PlayVars { pvSeconds       = 0
                                     , pvZoom          = 1
                                     , pvSelectedTile  = Nothing
                                     , pvTileContent   = Nothing
                                     , pvActivePlayer  = Just activePlayer
                                     , pvPlayerActions = []
                                     }
  --, pvDinoState         = DinoState DinoAction'Move Nothing Nothing Nothing
  --, pvDinoPos           = Animate.initPosition DinoKey'Move
  --, pvMountainPos       = Animate.initPosition MountainKey'Idle
  --, pvRiverPos          = Animate.initPosition RiverKey'Idle
  --, pvMountainScroll    = 0
  --, pvJungleScroll      = 0
  --, pvGroundScroll      = 0
  --, pvRiverScroll       = 0
  --, pvObstacles         = []
  --, pvUpcomingObstacles = upcomingObstacles

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

-- State of the program
data Model = Model
  { vGame :: Game
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
  game         = twoPlayersGame
  activePlayer = case game ^? (gamePlayers . ix 0) of
    Just p  -> p
    Nothing -> initPlayer "Empty" (CubeCoord (0, 0, 0))


toScene' :: MonadState Model m => SceneType -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

instance HasGame Model where
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
