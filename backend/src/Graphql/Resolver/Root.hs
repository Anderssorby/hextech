module Graphql.Resolver.Root where

import           Data.Morpheus.Types
import           Graphql
import           Graphql.Resolver.User
import           Graphql.Resolver.Game

rootResolver :: RootResolver Web () Query Mutation Undefined
rootResolver = RootResolver { queryResolver
                            , mutationResolver
                            , subscriptionResolver
                            }
 where
  queryResolver =
    Query { login = loginResolver, currentUser = myUserInfoResolver }
                        --, availableGames = availableGamesResolver
  mutationResolver = Mutation { register       = registerResolver
                              , changePassword = changePasswordResolver
                              , createGame     = createGameResolver
                              }
  subscriptionResolver = Undefined
