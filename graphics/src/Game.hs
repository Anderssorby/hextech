{-# LANGUAGE TemplateHaskell #-}
module Game
  ()
where

import           Grid                           ( Grid
                                                , hexagonGrid
                                                )
import           Control.Monad.Extra            ( whileM
                                                , fold1M
                                                , foldM
                                                )
import           Control.Lens


data PieceType = Leader | Drone | Tower | FastDrone deriving (Show)

pieceStrength :: PieceType -> Int
pieceStrength Leader    = 3
pieceStrength Drone     = 1
pieceStrength Tower     = 3
pieceStrength FastDrone = 1

data ResourceType = Pluss | Star deriving (Show)

type GridPosition = (Int, Int, Int)

data Resource = FreeResource ResourceType GridPosition deriving (Show)

data Piece = Piece {piecePosition :: GridPosition, pieceType :: PieceType} deriving (Show)

makeLenses ''Piece

data Player = Player {playerName :: String, playerPieces :: [Piece], playerResources :: [ResourceType]} deriving (Show)

makeLenses ''Player

data Game = Game {
    _gamePlayers :: [Player]
      , _freeResources :: [Resource]
      , _gameGrid :: Grid
                 } deriving (Show)

makeLenses ''Game

data Action
    = Move GridPosition
    | Excavate
    | BuildDrone GridPosition
    | UpgradePiece
    | Attack GridPosition

twoPlayersGame :: Game
twoPlayersGame = Game
  { _gamePlayers   = [ Player
                       { playerName      = "Anders"
                       , playerPieces    = [ Piece { piecePosition = (0, 0, 0)
                                                   , pieceType     = Leader
                                                   }
                                           ]
                       , playerResources = []
                       }
                     , Player
                       { playerName      = "Antoine"
                       , playerPieces    = [ Piece { piecePosition = (0, 0, 0)
                                                   , pieceType     = Leader
                                                   }
                                           ]
                       , playerResources = []
                       }
                     ]
  , _freeResources = []
  , _gameGrid      = hexagonGrid 5
  }

executePlayerAction :: Player -> Game -> Action -> Game
executePlayerAction player game (Move position) = game
executePlayerAction player game action          = game

executePlayerActions :: Game -> Player -> [Action] -> Game
executePlayerActions game player actions =
  foldl (executePlayerAction player) game actions

runRound :: Monad m => Game -> (Player -> m [Action]) -> m Game
runRound game getActions = do
  players <- return $ view gamePlayers game
  foldM (\g p -> runPlayerRound g getActions p) game players

runPlayerRound :: Monad m => Game -> (Player -> m [Action]) -> Player -> m Game
runPlayerRound game getActions player = do
  actions <- getActions player
  return $ executePlayerActions game player actions

runGame :: Monad m => Game -> (Player -> m [Action]) -> m Game
runGame game getActions = do
  game <- runRound game getActions
  runGame game getActions
