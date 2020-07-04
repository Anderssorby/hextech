{-# LANGUAGE TemplateHaskell #-}
module HexTech.State where

import           Control.Lens
import           HexTech.Game

data Vars = Vars
  { vGame :: Game
  , vScene :: Scene
  , vNextScene :: Scene
  , vTitle :: TitleVars
  , vPlay :: PlayVars
  , vGameOver :: GameOverVars
  , vInput :: Input
  , vCamera :: Camera
  } deriving (Show, Eq)

initVars :: [(Int, ObstacleTag)] -> Vars
initVars mkObstacles = Vars initCommonVars
                            Scene'Title
                            Scene'Title
                            initTitleVars
                            (initPlayVars mkObstacles)
                            initGameOverVars
                            initInput
                            initCamera

instance HasGame Vars where
  game = lens vGame (\v s -> v { vGame = s })
--instance HasCommonVars Vars where
--  commonVars = lens vCommon (\v s -> v { vCommon = s })
--
--instance HasTitleVars Vars where
--  titleVars = lens vTitle (\v s -> v { vTitle = s })
--
--instance HasPlayVars Vars where
--  playVars = lens vPlay (\v s -> v { vPlay = s })
--
--instance HasGameOverVars Vars where
--  gameOverVars = lens vGameOver (\v s -> v { vGameOver = s })

makeClassy ''Vars
