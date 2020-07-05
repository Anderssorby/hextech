module HexTech.Scene where

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'Death
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)

class Monad m => SceneManager m where
  toScene :: Scene -> m ()
