module Game
  ()
where

import           Grid                           ( Grid
                                                , hexagonGrid
                                                )

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

data Player = Player {playerName :: String, playerPieces :: [Piece], playerResources :: [ResourceType]} deriving (Show)

data Game = Game {players :: [Player], freeResources :: [Resource], gameGrid :: Grid} deriving (Show)

data Action
    = Move GridPosition
    | Excavate
    | BuildDrone GridPosition
    | UpgradePiece
    | Attack GridPosition

twoPlayersGame :: Game
twoPlayersGame = Game
  { players       = [ Player
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
  , freeResources = []
  , gameGrid      = hexagonGrid 5
  }

executePlayerAction :: Player -> Game -> Action -> Game
executePlayerAction player game action = game

executePlayerActions :: Game -> Player -> [Action] -> Game
executePlayerActions game player actions =
  foldl (executePlayerAction player) game actions
