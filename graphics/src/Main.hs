{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main
  ( main
  )
where

import           Control.Monad                  ( foldM )
import           Control.Monad.Extra            ( whileM )

import           Graphics                       ( makeWindow )
import           Game


main :: IO ()
main = do
  putStrLn "Main"
  makeWindow
