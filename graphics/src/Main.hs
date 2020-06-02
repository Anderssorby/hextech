{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main
  ( main
  )
where

import           Control.Monad                  ( foldM )
import           Control.Monad.Extra            ( whileM )

import           Graphics
import           Game


main :: IO ()
main = do
  makeWindow
