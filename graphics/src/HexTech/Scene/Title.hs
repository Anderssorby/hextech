{-# LANGUAGE TemplateHaskell #-}
module HexTech.Scene.Title where

import           Control.Lens
import           Control.Monad                  ( when )
import           Control.Monad.Reader           ( MonadReader(..) )
import           Control.Monad.State            ( MonadState(..)
                                                , modify
                                                , gets
                                                )
import           KeyState

import qualified HexTech.Engine.Types          as T
import           HexTech.Engine.Types           ( Logger(..)
                                                , Clock(..)
                                                )
import           HexTech.State                 as State
import           HexTech.Config                 ( Config(..) )
import           HexTech.Engine.Renderer        ( Renderer )
import qualified HexTech.Engine.Renderer       as Renderer
import qualified HexTech.Input                 as Input
import           HexTech.Input                  ( HasInput(..) )
import           HexTech.Engine.Audio           ( Audio(..) )


scene
  :: ( MonadState State.Model m
     , State.SceneManager m
     , Renderer m
     , State.CameraControl m
     , HasInput m
     , Audio m
     , Logger m
     , MonadReader Config m
     , Clock m
     )
  => State.Scene m
scene = State.Scene { drawScene       = drawTitle
                    , stepScene       = titleStep
                    , sceneTransition = titleTransition
                    }

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
     , State.SceneManager m
     )
  => m ()
titleStep = do
  input <- getInput
  when (ksStatus (Input.iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
  when (ksStatus (Input.iEscape input) == KeyStatus'Pressed)
       (toScene Scene'Quit)
  updateTitle
  drawTitle

titleTransition
  :: (HasTitleVars a, MonadState a m, CameraControl m, Audio m) => m ()
titleTransition = do
  stopGameMusic
  adjustCamera initCamera
  State.titleVars .= State.initTitleVars

updateTitle
  :: ( State.HasTitleVars s
     --, HasCommonVars s
     , MonadReader Config m
     , MonadState s m
     , Renderer m
     , HasInput m
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
  modify
    $  State.titleVars
    %~ (\tv -> tv { tvFlashing = State.tvFlashing tv + 0.025 })

drawTitle
  :: ( HasTitleVars s
     --, HasCommonVars s
     , MonadReader Config m
     , MonadState s m
     , Renderer m
     , HasInput m
     , State.SceneManager m
     )
     --, HUD m
  => m ()
drawTitle = do
  tv <- use titleVars
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

  when (titleShowPressSpace $ State.tvFlashing tv)
    $ Renderer.drawPressSpaceText (550, 500)
  when (titleShowPressEscape $ State.tvFlashing tv)
    $ Renderer.drawPressEscapeText (490, 500)
