module HexTech.Grid
  ( Grid(..)
  , GridArgs(..)
  , Tile(..)
  , AxialCoord(..)
  , CubeCoord(..)
  , getAxialCoords
  , hexagonGrid
  , toCubeCoord
  , toAxialCoords
  )
where

import           Control.Monad                  ( guard )
import qualified Data.Map.Strict               as Map
import           Control.Lens
import           HexTech.Engine.Types           ( Point(..)
                                                , p
                                                , (<+>)
                                                )


newtype AxialCoord = AxialCoord (Int, Int) deriving (Show, Eq, Ord)
newtype CubeCoord = CubeCoord (Int, Int, Int) deriving (Show, Eq, Ord)

data Tile = Tile {tileCoords :: CubeCoord, tileCorners :: [Point], tileCenter :: Point } deriving (Show, Eq)
makeLenses ''Tile


data Grid = Grid {gridTiles :: Map.Map CubeCoord Tile} deriving (Show, Eq)
makeLenses ''Grid

data GridArgs = GridArgs {gRadius :: Int, gPosition :: Point, gSize :: Int}

getAxialCoords :: CubeCoord -> (Int, Int)
getAxialCoords (CubeCoord (x, _y, z)) = (x, z)

--instance Show (Vector Point) where
--  show v = Vector.foldl (\str p -> str ++ ", ") "[" v ++ "]"
--
--instance Eq (Vector Point) where
--  v1 == v2 = Vector.all $ Vector.zipWith (==) v1 v2

toAxialCoords :: CubeCoord -> AxialCoord
toAxialCoords (CubeCoord (x, _, z)) = AxialCoord (x, z)

toCubeCoord :: AxialCoord -> CubeCoord
toCubeCoord (AxialCoord (q, r)) = CubeCoord (q, (-q) + (-r), r)


hexagonGrid :: GridArgs -> Grid
hexagonGrid args@(GridArgs { gRadius = radius, gSize }) = Grid
  { gridTiles = tiles
  }
 where
  coords = do
    x <- [(-radius) .. radius]
    y <- [(-radius) .. radius]
    z <- [(-radius) .. radius]
    guard $ x + y + z == 0
    return $ CubeCoord (x, y, z)
  tiles = Map.fromList $ map
    (\coord@(CubeCoord (x, _, z)) ->
      let cp = calcTileCorners args (x, z)
      in  ( coord
          , Tile { tileCoords  = coord
                 , tileCorners = makeHexagon cp gSize
                 , tileCenter  = cp
                 }
          )
    )
    coords

calcTileCorners :: GridArgs -> (Int, Int) -> Point
calcTileCorners (GridArgs { gSize = size, gPosition }) (x, y) =
  (p px py <+> gPosition)
 where
  lineWidth = 0
  xOffset   = sqrt 3 * fromIntegral size :: Float
  yOffset   = 1.5 * fromIntegral size :: Float
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
