{-# LANGUAGE TemplateHaskell #-}
module HexTech.Game
  ( twoPlayersGame
  , Action(..)
  , Player(..)
  , Game
  , Piece
  , Resource(..)
  , PieceType
  --, runGame
  --, runRound
  , HasGame(..)
  )
where

import           HexTech.Grid                   ( Grid
                                                , hexagonGrid
                                                )
--import           Control.Monad.Extra            ( whileM
--                                                , fold1M
--                                                , foldM
--                                                )
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

makeClassy ''Player

data Game = Game
      { _gamePlayers :: [Player]
      , _playerPieces :: Map Player [Piece]
      , _piecePositions :: Map Piece GridPosition
      --, playerResources :: Map Player [ResourceType]
      --, playerNames :: Map Player String
      , _freeResources :: [Resource]
      , _gameGrid :: Grid
      } deriving (Show, Eq)


makeClassy ''Game

movePiece :: Piece -> GridPosition -> State Game ()
movePiece piece pos = do
  gameState <- get
  put
    (set piecePositions
         (Map.insert piece pos $ view piecePositions gameState)
         gameState
    )
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
        { _gamePlayers    = players
        , _playerPieces   = Map.fromList
                              [(anders, [leaderPiece]), (antoine, [leaderPiece])]
        , _piecePositions = Map.fromList
          [(leaderPiece, (1, 0, 0)), (leaderPiece, (2, 3, 1))]
        , _freeResources  = []
        , _gameGrid       = hexagonGrid 5
        }

--executePlayerAction :: Player -> Action -> State Game ()
--executePlayerAction player (Move position) = return ()
--executePlayerAction player action          = return ()
--
--executePlayerActions :: Player -> [Action] -> State Game ()
--executePlayerActions player actions =
--  foldM (\_ action -> executePlayerAction player action) () actions
--
--runRound :: (Player -> [Action]) -> State Game ()
--runRound getActions = do
--  game <- get
--  let players = game ^. gamePlayers
--  foldM (\_ p -> runPlayerRound getActions p) () players
--
--runPlayerRound :: (Player -> [Action]) -> Player -> State Game ()
--runPlayerRound getActions player = do
--  game <- get
--  let actions = getActions player
--  executePlayerActions player actions
--  return ()
--
--runGame :: (Player -> [Action]) -> State Game ()
--runGame getActions = do
--  game <- get
--  runRound getActions
--  runGame getActions
