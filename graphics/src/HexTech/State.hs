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

import           HexTech.Game                   ( Game
                                                , HasGame(..)
                                                , Player(..)
                                                , twoPlayersGame
                                                )
import           HexTech.Grid                   ( Tile(..) )

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
  , pvActivePlayer :: Maybe Player
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

initPlayVars :: PlayVars
initPlayVars = PlayVars { pvSeconds      = 0
                        , pvZoom         = 1
                        , pvSelectedTile = Nothing
                        , pvActivePlayer = Nothing
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
initSettings = Settings { _sMuted = False, _sShowCoords = True }

modifySettings
  :: (MonadState s m, HasSettings s) => (Settings -> Settings) -> m ()
modifySettings f = modify $ settings %~ f

data Vars = Vars
  { vGame :: Game
  , vScene :: SceneType
  , vNextScene :: SceneType
  , vTitle :: TitleVars
  , vPlay :: PlayVars
  --, vGameOver :: GameOverVars
  , vInput :: Input
  , vCamera :: Camera
  , vSettings :: Settings
  } deriving (Show, Eq)


makeClassy_ ''Vars

initVars :: Vars
initVars = Vars twoPlayersGame
                Scene'Title
                Scene'Title
                initTitleVars
                initPlayVars
                initInput
                initCamera
                initSettings


toScene' :: MonadState Vars m => SceneType -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

instance HasGame Vars where
  game = lens vGame (\v s -> v { vGame = s })

instance HasSettings Vars where
  settings = lens vSettings (\v s -> v { vSettings = s })

--instance HasCommonVars Vars where
--  commonVars = lens vCommon (\v s -> v { vCommon = s })
--
instance HasTitleVars Vars where
  titleVars = lens vTitle (\v s -> v { vTitle = s })

instance HasPlayVars Vars where
  playVars = lens vPlay (\v s -> v { vPlay = s })
--
--instance HasGameOverVars Vars where
--  gameOverVars = lens vGameOver (\v s -> v { vGameOver = s })

adjustCamera'
  :: (MonadIO m, MonadReader Config m, MonadState Vars m) => Camera -> m ()
adjustCamera' cam = do
  modify $ \v -> set _vCamera cam v
  renderer <- asks cRenderer
  moveCamera renderer cam

disableZoom' :: (MonadIO m, MonadReader Config m) => m ()
disableZoom' = do
  renderer <- asks cRenderer
  moveCamera renderer initCamera

enableZoom' :: (MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
enableZoom' = do
  renderer <- asks cRenderer
  cam      <- gets vCamera
  moveCamera renderer cam

getInput' :: MonadState Vars m => m Input
getInput' = gets vInput

setInput' :: MonadState Vars m => Input -> m ()
setInput' input = modify (\v -> v { vInput = input })
