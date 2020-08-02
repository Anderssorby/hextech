module HexTech.API where

import           Data.Morpheus.Client

schema = defineByDocumentFile "backend/schema.graphql"
