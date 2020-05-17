{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main
  ( main
  )
where

import qualified SDL
import           SDL                            ( ($=) )
import qualified Common                        as C

import           Control.Monad                  ( foldM )
import           Control.Monad.Extra            ( whileM )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Vector.Storable          as Vector
import           Data.Vector.Storable           ( Vector )
import           Prelude                 hiding ( Left
                                                , Right
                                                )

import           Foreign.C.Types                ( CInt(..) )

import           Graphics
import           Game


main :: IO ()
main = makeWindow
