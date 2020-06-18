{-# LANGUAGE TemplateHaskell #-}
module Game
  ( twoPlayersGame
  , Action(..)
  , Player
  , Game
  , Piece
  , Resource(..)
  , PieceType
  , runGame
  , runRound
  )
where

import           Grid                           ( Grid
                                                , hexagonGrid
                                                )
import           Control.Monad.Extra            ( whileM
                                                , fold1M
                                                , foldM
                                                )
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )


data PieceType = Leader | Drone | Tower | FastDrone deriving (Show, Eq, Ord)

pieceStrength :: PieceType -> Int
pieceStrength Leader    = 3
pieceStrength Drone     = 1
pieceStrength Tower     = 3
pieceStrength FastDrone = 1

data ResourceType = Plus | Star deriving (Show, Eq)

type GridPosition = (Int, Int, Int)

data Resource = FreeResource ResourceType GridPosition deriving (Show, Eq)

data Piece = Piece
    {
    --piecePosition :: GridPosition
     pieceType :: PieceType
    } deriving (Show, Eq, Ord)

makeLenses ''Piece

data Player = Player
    { playerName :: String
    --, playerPieces :: [Piece]
    --, playerResources :: [ResourceType]
    } deriving (Show, Eq, Ord)

makeLenses ''Player

data Game = Game
      { gamePlayers :: [Player]
      , playerPieces :: Map Player [Piece]
      , piecePositions :: Map Piece GridPosition
      --, playerResources :: Map Player [ResourceType]
      --, playerNames :: Map Player String
      , freeResources :: [Resource]
      , gameGrid :: Grid
      } deriving (Show)


type GameState = State Game

makeLenses ''Game

movePiece :: Piece -> GridPosition -> State Game ()
movePiece piece pos = do
  game <- get
  put (game { piecePositions = Map.insert piece pos $ piecePositions game })
  return ()

data Action
    = Move GridPosition
    | Excavate
    | BuildDrone GridPosition
    | UpgradePiece
    | Attack GridPosition

twoPlayersGame :: Game
twoPlayersGame =

  let anders      = Player { playerName = "Anders" }
      antoine     = Player { playerName = "Antoine" }
      leaderPiece = Piece { pieceType = Leader }
      players     = [anders, antoine]
  in  Game
        { gamePlayers    = players
        , playerPieces   = Map.fromList
                             [(anders, [leaderPiece]), (antoine, [leaderPiece])]
        , piecePositions = Map.fromList
          [(leaderPiece, (1, 0, 0)), (leaderPiece, (2, 3, 1))]
        , freeResources  = []
        , gameGrid       = hexagonGrid 5
        }

executePlayerAction :: Player -> Action -> State Game ()
executePlayerAction player (Move position) = return ()
executePlayerAction player action          = return ()

executePlayerActions :: Player -> [Action] -> State Game ()
executePlayerActions player actions =
  foldM (\_ action -> executePlayerAction player action) () actions

runRound :: (Player -> [Action]) -> State Game ()
runRound getActions = do
  game <- get
  let players = gamePlayers game
  foldM (\_ p -> runPlayerRound getActions p) () players

runPlayerRound :: (Player -> [Action]) -> Player -> State Game ()
runPlayerRound getActions player = do
  game <- get
  let actions = getActions player
  executePlayerActions player actions
  return ()

runGame :: (Player -> [Action]) -> State Game ()
runGame getActions = do
  game <- get
  runRound getActions
  runGame getActions
