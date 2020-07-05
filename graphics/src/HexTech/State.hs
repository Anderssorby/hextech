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
                                                , twoPlayersGame
                                                )

import           HexTech.Config                 ( Config(..) )
import           HexTech.Scene                  ( Scene(..) )
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

data Vars = Vars
  { vGame :: Game
  , vScene :: Scene
  , vNextScene :: Scene
  , vTitle :: TitleVars
  --, vPlay :: PlayVars
  --, vGameOver :: GameOverVars
  , vInput :: Input
  , vCamera :: Camera
  } deriving (Show, Eq)

makeClassy_ ''Vars

initVars :: Vars
initVars =
  Vars twoPlayersGame Scene'Title Scene'Title initTitleVars initInput initCamera


toScene' :: MonadState Vars m => Scene -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

instance HasGame Vars where
  game = lens vGame (\v s -> v { vGame = s })

--instance HasCommonVars Vars where
--  commonVars = lens vCommon (\v s -> v { vCommon = s })
--
instance HasTitleVars Vars where
  titleVars = lens vTitle (\v s -> v { vTitle = s })
--
--instance HasPlayVars Vars where
--  playVars = lens vPlay (\v s -> v { vPlay = s })
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
