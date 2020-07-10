module HexTech.Scene where

--import           Control.Monad.State            ( MonadState )
--import           HexTech.Engine.Audio           ( Audio )
--import           HexTech.Engine.Types           ( Logger )
--import           HexTech.Engine.Renderer        ( Renderer )


data SceneType
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)

--data Scene where
--    Scene :: {
--    drawScene :: (MonadState s m, Renderer m) => m ()
--   ,stepScene :: (MonadState s m, Renderer m) => m ()
--   ,sceneTransition :: (MonadState s m, Audio m, Logger m) => m ()
--   }


class Monad m => SceneManager m where
  toScene :: SceneType -> m ()
