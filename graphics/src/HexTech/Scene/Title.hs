{-# LANGUAGE TemplateHaskell #-}
module HexTech.Scene.Title where

--import qualified Animate
import           Control.Lens
import           Control.Monad                  ( when )
import           Control.Monad.Reader           ( MonadReader(..) )
import           Control.Monad.State            ( MonadState(..)
                                                , modify
                                                , gets
                                                )
import           KeyState

import           HexTech.Config                 ( Config(..) )
--import           HexTech.Engine.Types
--import           HexTech.Engine.Common
import           HexTech.Engine.Renderer        ( Renderer )
import qualified HexTech.Engine.Renderer       as Renderer
import           HexTech.Input                  ( HasInput(..)
                                                , iSpace
                                                )
import           HexTech.Scene

data TitleVars = TitleVars
  {
  {-tvDinoPos :: Animate.Position DinoKey Seconds
  , tvMountainPos :: Animate.Position MountainKey Seconds
  , tvRiverPos :: Animate.Position RiverKey Seconds
  , -}
  tvFlashing :: Float

  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars 0

titleShowPressSpace :: Float -> Bool
titleShowPressSpace p = sin p > 0.5

titleShowPressEscape :: Float -> Bool
titleShowPressEscape p = sin p < -0.5

titleStep
  :: ( HasTitleVars s
     , MonadReader Config m
     , MonadState s m
     , Renderer m
     , HasInput m
     , SceneManager m
     )
  => m ()
titleStep = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
  updateTitle
  drawTitle

updateTitle
  :: ( HasTitleVars s
     --, HasCommonVars s
     , MonadReader Config m
     , MonadState s m
     , Renderer m
     , HasInput m
     , SceneManager m
     )
     --, AudioSfx m
  => m ()
updateTitle = do
  --dinoAnimations <- getDinoAnimations
  --dinoPos        <- gets (tvDinoPos . view titleVars)
  --let dinoPos' = Animate.stepPosition dinoAnimations dinoPos frameDeltaSeconds
  --
  --mountainAnimations <- getMountainAnimations
  --mountainPos        <- gets (tvMountainPos . view titleVars)
  --let mountainPos' =
  --      Animate.stepPosition mountainAnimations mountainPos frameDeltaSeconds
  --
  --riverAnimations <- getRiverAnimations
  --riverPos        <- gets (tvRiverPos . view titleVars)
  --let riverPos' =
  --      Animate.stepPosition riverAnimations riverPos frameDeltaSeconds
  --
  modify $ titleVars %~ (\tv -> tv { tvFlashing = tvFlashing tv + 0.025 })

drawTitle
  :: ( HasTitleVars s
     --, HasCommonVars s
     , MonadReader Config m
     , MonadState s m
     , Renderer m
     , HasInput m
     , SceneManager m
     )
     --, HUD m
  => m ()
drawTitle = do
  tv <- gets (view titleVars)
  --quake              <- gets (cvQuake . view commonVars)
  --
  --mountainAnimations <- getMountainAnimations
  --let mountainPos = tvMountainPos tv
  --let mountainLoc = Animate.currentLocation mountainAnimations mountainPos
  --drawMountain mountainLoc $ applyQuakeToMountain quake (0, mountainY)
  --
  --drawJungle $ applyQuakeToJungle quake (0, jungleY)
  --drawGround $ applyQuakeToGround quake (0, groundY)
  --
  --riverAnimations <- getRiverAnimations
  --let riverPos = tvRiverPos tv
  --let riverLoc = Animate.currentLocation riverAnimations riverPos
  --drawRiver riverLoc $ applyQuakeToRiver quake (0, riverY)
  --
  --dinoAnimations <- getDinoAnimations
  --let dinoPos = tvDinoPos tv
  --let dinoLoc = Animate.currentLocation dinoAnimations dinoPos
  --drawDino dinoLoc $ applyQuakeToGround quake (truncate dinoX, dinoY)
  --
  --drawHiscore

  Renderer.drawTitleText (300, 180)

  when (titleShowPressSpace $ tvFlashing tv)
    $ Renderer.drawPressSpaceText (550, 500)
  when (titleShowPressEscape $ tvFlashing tv)
    $ Renderer.drawPressEscapeText (490, 500)
