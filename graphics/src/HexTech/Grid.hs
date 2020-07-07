module HexTech.Grid
  ( Grid(..)
  , Tile
  , AxialCoord(..)
  , getAxialCoords
  , hexagonGrid
  , toCubeCoord
  , toAxialCoords
  , tileCoords
  )
where

import           Control.Monad                  ( guard )
import           Data.List                      ( groupBy )
import           Control.Lens

data Grid = Grid {gridTiles :: [[Tile]]} deriving (Show, Eq)


newtype Point = Point (Int, Int) deriving (Show, Eq)

newtype AxialCoord = AxialCoord (Int, Int) deriving (Show, Eq)
newtype CubeCoord = CubeCoord (Int, Int, Int) deriving (Show, Eq)

getAxialCoords :: CubeCoord -> (Int, Int)
getAxialCoords (CubeCoord (x, _y, z)) = (x, z)

--instance Show (Vector Point) where
--  show v = Vector.foldl (\str p -> str ++ ", ") "[" v ++ "]"
--
--instance Eq (Vector Point) where
--  v1 == v2 = Vector.all $ Vector.zipWith (==) v1 v2

data Tile = Tile {tileCoords :: CubeCoord, tileCorners :: [Point] } deriving (Show, Eq)


makeClassy_ ''Tile

toAxialCoords :: CubeCoord -> AxialCoord
toAxialCoords (CubeCoord (x, _, z)) = AxialCoord (x, z)

toCubeCoord :: AxialCoord -> CubeCoord
toCubeCoord (AxialCoord (q, r)) = CubeCoord (q, (-q) + (-r), r)


hexagonGrid :: Int -> Grid
hexagonGrid radius = Grid { gridTiles = grid }
 where
  coords = do
    x <- [(-radius) .. radius]
    y <- [(-radius) .. radius]
    z <- [(-radius) .. radius]
    guard $ x + y + z == 0
    return (x, y, z)
  flat = map
    (\(x, y, z) -> Tile { tileCoords  = CubeCoord (x, y, z)
                        , tileCorners = calcTileCorners (x, z) (450, 450)
                        }
    )
    coords
  grid = groupBy
    (\t1 t2 ->
      ((getAxialCoords . tileCoords) t1)
        ^. _1
        == ((getAxialCoords . tileCoords) t2)
        ^. _1
    )
    flat

calcTileCorners :: (Int, Int) -> (Int, Int) -> [Point]
calcTileCorners (x, y) (gx, gy) = makeHexagon (Point (gx + px, gy + py))
  $ round size
 where
  lineWidth = 0
  size      = 50 :: Float
  xOffset   = sqrt 3 * size :: Float
  yOffset   = 1.5 * size :: Float
  py = round $ fromIntegral x + yOffset * fromIntegral y - 2 * lineWidth :: Int
  px =
    ( round
    $ fromIntegral x
    + xOffset
    * (fromIntegral x + 0.5 * fromIntegral (y `rem` 2 :: Int))
    - 2
    * lineWidth
    ) :: Int


makeHexagon :: Point -> Int -> [Point]
makeHexagon center size = map (pointyHexCorner center size) [0 .. 7]

pointyHexCorner :: Point -> Int -> Int -> Point
pointyHexCorner (Point (x, y)) size i = Point (px, py)
 where
  angleRad = pi / 3.0 * fromIntegral i + pi / 6.0 :: Double
  px       = round (fromIntegral x + fromIntegral size * cos (angleRad))
  py       = round (fromIntegral y + fromIntegral size * sin (angleRad))
