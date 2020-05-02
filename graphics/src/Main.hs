{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C


main :: IO ()
main = C.withSDL $ C.withWindow "HexTech" (1000, 800) $
  \w -> do

    screen <- SDL.getWindowSurface w
    pixelFormat <- SDL.surfaceFormat `applyToPointer` screen
    --color <- SDL.mapRGB pixelFormat 0xFF 0xFF 0xFF
    SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound maxBound maxBound maxBound)
    SDL.updateWindowSurface w

    SDL.delay 2000

    SDL.freeSurface screen
