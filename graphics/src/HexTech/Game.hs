{-# LANGUAGE TemplateHaskell #-}
module HexTech.Game
  ( twoPlayersGame
  , Action(..)
  , Player(..)
  , playerName
  , playerPieces
  , playerResources
  , Game
  , Piece(..)
  , piecePosition
  , pieceStrength
  , pieceType
  , Resource(..)
  , ResourceType(..)
  , PieceType(..)
  --, movePiece
  --, runGame
  --, runRound
  , HasGame(..)
  )
where

import           Data.Text
import           HexTech.Grid                   ( Grid(..)
                                                , GridArgs(..)
                                                , CubeCoord(..)
                                                , cubeCoord
                                                , hexagonGrid
                                                )
import qualified HexTech.Engine.Types          as T
--import           Control.Monad.Extra            ( whileM
--                                                , fold1M
--                                                , foldM
--                                                )
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )


data PieceType = Commander | Drone | Tower | FastDrone deriving (Show, Eq, Ord)

pieceStrength :: PieceType -> Int
pieceStrength Commander = 3
pieceStrength Drone     = 1
pieceStrength Tower     = 3
pieceStrength FastDrone = 1

data ResourceType = Plus | Star deriving (Show, Eq, Enum, Ord)

type GridPosition = CubeCoord

data Resource = FreeResource ResourceType GridPosition deriving (Show, Eq)

data Piece = Piece
    { _piecePosition :: GridPosition
    , _pieceType :: PieceType
    } deriving (Show, Eq, Ord)

makeLenses ''Piece

data Player = Player
    { _playerName :: Text
    , _playerPieces :: [Piece]
    , _playerResources :: [ResourceType]
    } deriving (Show, Eq)

makeLenses ''Player

data Game = Game
      { _gamePlayers :: [Player]
      --, _playerPieces :: Map Player [Piece]
      --, _piecePositions :: Map Piece GridPosition
      --, playerResources :: Map Player [ResourceType]
      --, playerNames :: Map Player String
      , _freeResources :: [Resource]
      , _gameGrid :: Grid
      } deriving (Show, Eq)


makeClassy ''Game

--movePiece :: Piece -> GridPosition -> State Game ()
--movePiece piece pos = do
--  gameState <- get
--  put
--    (set piecePositions
--         (Map.insert piece pos $ view piecePositions gameState)
--         gameState
--    )
--  return ()

data Action
    = Move GridPosition
    | Excavate
    | BuildDrone GridPosition
    | UpgradePiece
    | Attack GridPosition
    deriving (Show, Eq)

initPlayer :: Text -> CubeCoord -> Player
initPlayer name corner = Player
  { _playerName = name
  , _playerPieces = [Piece { _pieceType = Commander, _piecePosition = corner }]
  , _playerResources = []
  }


twoPlayersGame :: Game
twoPlayersGame =

  let anders   = initPlayer "Anders" $ CubeCoord (5, 0, -5)
      antoine  = initPlayer "Antoine" $ CubeCoord (-5, 0, 5)
      players  = [anders, antoine]
      gridArgs = GridArgs { gRadius = 5, gSize = 50, gPosition = T.p 600 450 }
  in  Game { _gamePlayers   = players
           , _freeResources = []
           , _gameGrid      = hexagonGrid gridArgs
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
