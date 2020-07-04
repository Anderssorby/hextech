{-# LANGUAGE TemplateHaskell #-}
module HexTech.Engine.Vars where

import qualified Animate
import           Control.Lens

--import DinoRush.Engine.Dino
--import DinoRush.Engine.Mountain
--import DinoRush.Engine.River
--import DinoRush.Engine.Obstacle
import           HexTech.Engine.Types

data PlayVars = PlayVars
  {
  --pvScore :: Score
  --, pvStocks :: Stocks
  --, pvSpeed :: Percent
  --, pvSeconds :: Seconds
   pvZoom :: Float
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

makeClassy ''PlayVars

initPlayVars :: PlayVars
initPlayVars = PlayVars { pvZoom = 1 }
