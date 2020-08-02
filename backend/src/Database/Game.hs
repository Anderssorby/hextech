module Database.Game where

import Control.Arrow (returnA)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Database.Base
import GHC.Int (Int64)
import Opaleye

-------------------------------------------------------------------------------
data GameT a b
  = Game
      { gameId :: a,
        gameName :: b
      }

$(makeAdaptorAndInstance "pGame" ''GameT)

type Game =
  Entity
    ( GameT
        Int
        Text
    )

type GameWriteField =
  EntityWriteField
    ( GameT
        (Maybe (F SqlInt4)) -- use Maybe because we don't need to specify id when inserting
        (F SqlText)
    )

type GameField =
  EntityField
    ( GameT (F SqlInt4)
        (F SqlText)
    )

gameTable :: Table GameWriteField GameField
gameTable =
  table "games" . pEntity . withTimestampFields $
    pGame
      Game
        { gameId = tableField "id",
          gameName = tableField "name"
        }
-------------------------------------------------------------------------------
type GameID = Int

-------------------------------------------------------------------------------
gameSelect :: Select GameField
gameSelect = selectTable gameTable

-------------------------------------------------------------------------------
insertGame :: (Text) -> Insert [GameID]
insertGame (gameName) =
  Insert
    { iTable = gameTable,
      iRows =
        withTimestamp
          [ Game
              { gameId = Nothing,
                gameName = toFields gameName
              }
          ],
      iReturning = rReturning (\((Entity (Game {gameId}) _ _)) -> gameId),
      iOnConflict = Nothing
    }

-------------------------------------------------------------------------------
findGameByName :: Text -> Select GameField
findGameByName name =
  proc () -> do
    game <- gameSelect -< ()
    let gameDetail = record game
    restrict -< gameName gameDetail .== toFields name
    returnA -< game

-------------------------------------------------------------------------------
findGameByID :: GameID -> Select GameField
findGameByID id =
  proc () -> do
    game <- gameSelect -< ()
    let gameDetail = record game
    restrict -< gameId gameDetail .== toFields id
    returnA -< game
