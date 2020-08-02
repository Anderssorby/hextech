{-# LANGUAGE RankNTypes #-}

module Graphql.Resolver.Game where

import           Authentication.JWT
import           Authentication.Password
import           Config
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Control.Monad.Trans            ( MonadIO
                                                , MonadTrans
                                                , liftIO
                                                )
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocument )
import           Data.Morpheus.Types
import           Data.Morpheus.Types.Internal.AST
                                                ( OperationType )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Base
import qualified Database.Model                as DB
import           Database.PostgreSQL.Simple     ( Connection )
import           GHC.Int                        ( Int64 )
import           Graphql

-------------------------------------------------------------------------------
gameResolver :: GraphQL o => DB.Game -> Value o Game
gameResolver game =
  let DB.Game { gameId, gameName } = record game
  in  return Graphql.Game { id = pure gameId, name = pure gameName }
                      --, createdAt = pure . T.pack . show $ recordCreatedAt game
                      --, updatedAt = pure . T.pack . show $ recordUpdatedAt game
-------------------------------------------------------------------------------
--availableGamesResolver :: GraphQL o => DB.Game -> Value o [Game]
--availableGamesResolver game =
--  let DB.Game { gameId, gameName } = record game
--  in  return [Graphql.Game { id = pure gameId, name = pure gameName }]
-------------------------------------------------------------------------------
createGameResolver :: CreateGameArgs -> Value MUTATION Game
createGameResolver CreateGameArgs { name } = do
  res :: [DB.Game] <- runSelect $ DB.findGameByName name
  case res of
    _ : _ -> fail "This name is already taken"
    []    -> do
      [gameId] <- runInsert $ DB.insertGame (name)
      runSelectOne (DB.findGameByID gameId) "Invalid game" >>= gameResolver
