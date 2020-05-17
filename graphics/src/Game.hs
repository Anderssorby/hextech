module Game
  ()
where


data PieceType = Leader | Drone | Tower | FastDrone

pieceStrength :: PieceType -> Int
pieceStrength Leader    = 3
pieceStrength Drone     = 1
pieceStrength Tower     = 3
pieceStrength FastDrone = 1

data ResourceType = Pluss | Star

type GridPosition = (Int, Int, Int)

data Resource = FreeResource ResourceType GridPosition

data Piece = Piece {piecePosition :: GridPosition, pieceType :: PieceType}

data Player = Player {playerName :: String, playerPieces :: [Piece], playerResources :: ResourceType -> Int}

data Game = Game {players :: [Player], freeResources :: [Resource]}

data Action
    = Move GridPosition
    | Excavate
    | BuildDrone GridPosition
    | UpgradePiece
    | Attack GridPosition

twoPlayersGame :: Game
twoPlayersGame = Game
  { players = [ Player
                { playerName      = "Anders"
                , playerPieces    = [ Piece { piecePosition = (0, 0, 0)
                                            , pieceType     = Leader
                                            }
                                    ]
                , playerResources = \_ -> 0
                }
              , Player
                { playerName      = "Antoine"
                , playerPieces    = [ Piece { piecePosition = (0, 0, 0)
                                            , pieceType     = Leader
                                            }
                                    ]
                , playerResources = \_ -> 0
                }
              ]
  }

executePlayerAction :: Player -> Game -> Action -> Game
executePlayerAction player game action = game

executePlayerActions :: Game -> Player -> [Action] -> Game
executePlayerActions game player actions =
  foldl (executePlayerAction player) game actions
