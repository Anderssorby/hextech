module Grid
  ( Grid
  , hexagonGrid
  , toCubeCoord
  )
where

import           Control.Monad                  ( guard )
import           Data.List                      ( groupBy )

data Grid = Grid [[Tile]] deriving (Show, Eq)


data AxialCoord = AxialCoord (Int, Int) deriving (Show, Eq)

data Tile = Tile AxialCoord deriving (Show, Eq)

toCubeCoord :: AxialCoord -> (Int, Int, Int)
toCubeCoord (AxialCoord (q, r)) = (q, r, (-q) + (-r))


hexagonGrid :: Int -> Grid
hexagonGrid radius = Grid $ grid
 where
  coords = do
    x <- [(-radius) .. radius]
    y <- [(-radius) .. radius]
    z <- [(-radius) .. radius]
    guard $ x + y + z == 0
    return (x, y, z)
  flat = map (\(x, y, z) -> Tile $ AxialCoord (x, y)) coords
  grid = groupBy
    (\(Tile (AxialCoord (q1, _r1))) (Tile (AxialCoord (q2, _r2))) -> q1 == q2)
    flat
