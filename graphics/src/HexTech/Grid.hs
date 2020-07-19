module HexTech.Grid
  ( Grid(..)
  , gTiles
  , GridArgs(..)
  , Tile(..)
  , AxialCoord(..)
  , CubeCoord(..)
  , Direction(..)
  , directionShift
  , cubeCoord
  , (+>)
  , ccToTuple
  , getAxialCoords
  , hexagonGrid
  , toCubeCoord
  , toAxialCoords
  , neighbours
  , pointToTile
  , tCoords
  , tCorners
  , tCenter
  )
where

import           Control.Monad                  ( guard )
import qualified Data.Map.Strict               as Map
import           Control.Lens
import           HexTech.Engine.Types           ( Point(..)
                                                , p
                                                , (<+>)
                                                , (<->)
                                                )


newtype AxialCoord = AxialCoord (Int, Int) deriving (Show, Eq, Ord)
newtype CubeCoord = CubeCoord (Int, Int, Int) deriving (Show, Eq, Ord)

cubeCoord :: Int -> Int -> Int -> Maybe CubeCoord
cubeCoord x y z | x + y + z == 0 = Just $ CubeCoord (x, y, z)
                | otherwise      = Nothing

ccToTuple :: CubeCoord -> (Int, Int, Int)
ccToTuple (CubeCoord coords) = coords

(+>) :: CubeCoord -> (Int, Int, Int) -> CubeCoord
(CubeCoord (x, y, z)) +> (sx, sy, sz) = CubeCoord (x + sx, y + sy, z + sz)


data Tile =
    Tile
        { tileCoords :: CubeCoord
        , tileCorners :: [Point]
        , tileCenter :: Point
        } deriving (Show, Eq)
makeLensesFor [("tileCoords", "tCoords"), ("tileCorners", "tCorners"), ("tileCenter", "tCenter")] ''Tile


data GridArgs = GridArgs {gRadius :: Int, gPosition :: Point, gSize :: Int} deriving (Show, Eq)

data Grid = Grid {gridTiles :: Map.Map CubeCoord Tile, gridArgs :: GridArgs} deriving (Show, Eq)
makeLensesFor [("gridTiles", "gTiles"), ("gridArgs", "gArgs")] ''Grid

getAxialCoords :: CubeCoord -> (Int, Int)
getAxialCoords (CubeCoord (x, _y, z)) = (x, z)

data Direction
    = NorthEast
    | NorthWest
    | West
    | SouthWest
    | SouthEast
    | East
    deriving (Show, Eq, Enum, Bounded)

directionShift :: Direction -> (Int, Int, Int)
directionShift NorthEast = (1, 0, -1)
directionShift NorthWest = (0, 1, -1)
directionShift West      = (-1, 1, 0)
directionShift SouthWest = (-1, 0, 1)
directionShift SouthEast = (0, -1, 1)
directionShift East      = (1, -1, 0)


neighbours :: Tile -> [CubeCoord]
neighbours (Tile { tileCoords = coords }) = do
  direction <- [minBound .. maxBound] :: [Direction]

  return $ coords +> directionShift direction


pointToTile :: Point -> Grid -> Maybe Tile
pointToTile point grid = grid ^. (gTiles . at (CubeCoord (q, (-q) - r, r)))
 where
  (Point (x, y)) = point <-> (gPosition $ grid ^. gArgs)
  fi             = fromIntegral
  size           = fromIntegral (gSize $ grid ^. gArgs) :: Float
  q              = round $ (sqrt 3 / 3 * fi x - 1 / 3 * fi y) / size
  r              = round $ (2 / 3 * fi y) / size


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
  , gridArgs  = args
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
      let cp = calcTilePosition args (x, z)
      in  ( coord
          , Tile { tileCoords  = coord
                 , tileCorners = makeHexagon cp gSize
                 , tileCenter  = cp
                 }
          )
    )
    coords

calcTilePosition :: GridArgs -> (Int, Int) -> Point
calcTilePosition (GridArgs { gSize = size, gPosition }) (q, r) =
  (p px py <+> gPosition)
 where
  lineWidth = 1
  py        = round $ fromIntegral (size * r) * 3 / 2 - 2 * lineWidth :: Int
  px =
    ( round
    $ fromIntegral size
    * (sqrt 3 * fromIntegral q + sqrt 3 / 2 * fromIntegral r)
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
