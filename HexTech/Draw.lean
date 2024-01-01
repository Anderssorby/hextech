import HexTech.Basic
import HexTech.Grid
import SDL
open SDL

namespace HexTech

structure DrawData where
  cont : Bool := true
  point : Point := {x := 0, y := 0}
  showCoords : Bool
  debugLog : Bool

def DrawM := StateT DrawData IO
  deriving Monad, MonadState

/-- Draw the game play view
-/

def drawPlay : DrawM Unit := do
  let pv <- use playVars
  disableZoom
  grid <- use (game . gameGrid)
  drawGrid grid
  players       <- use (game . gamePlayers)
  mActivePlayer <- use (playVars . _pvActivePlayer)
  let tilePos pos =
        maybe (T.p 450 450) tileCenter $ Map.lookup pos (gridTiles grid)
  mapM_
    (\(i, player) -> do
      let
        name   = player ^. playerName
        active = maybe False
                       (\p -> player ^. playerName == p ^. playerName)
                       mActivePlayer
        text =
          T.showText (i + 1) <> ". " <> name <> (if active then " *" else "")
      drawText text (T.p 50 (100 + 28 * i))

      mapM_
        (\piece -> do
          let piecePoint = tilePos $ view piecePosition piece
          drawPiece piece piecePoint
        )
        (view playerPieces player)
    )
    (NonEmpty.zip (0 :| [1 ..]) players)

  resources <- use (game . freeResources)
  mapM_
    (\(pos, res) -> do
      let posPoint = tilePos pos
          text     = case res of
            Star -> "*"
            Plus -> "+"

      drawText text (posPoint <+> T.p (-12) (-10))
    )
    (Map.toList resources)

  drawControlsText (10, 710)
  drawDigits (secondsToInteger $ pvSeconds pv) (T.p 50 50)
  muted <- gets $ view (settings . sMuted)
  let muteSprite | muted     = (rMuted . cResources)
                 | otherwise = (rUnMuted . cResources)

  drawTextureSprite muteSprite (1000, 50)

  mContent <- use (playVars . _pvTileContent)
  case mContent of
    Just content -> drawTileContent content (T.p 1000 80)
    Nothing      -> return ()

  enableZoom


def drawTileContent (content: Game.TileContent) (point : Point) : GameM Unit := do
  drawText "Tile Contains:" point

  match content.tileResource with
  | some res => drawText ("Res: " |> T.showText res) (point + { x := 10, y := 30})
  | none  => return ()

  let pieces := content.tilePieces

  for (i, p) in pieces
    drawText (T.showText (p.pieceType))
                         (point + { x := 10, y := (30 * i + 60)})

    (zip [0 ..] pieces)

def Piece.draw (piece : Piece) (point : Point): DrawM Unit := do

  drawCommander Commander'Idle ({ x := -15, y := -28} + point)


def Commander.draw (key : CommanderKey) (pos : Point) : DrawM Unit := do
  let animations <- getSpriteAnimations (rCommanderSprites . cResources)
  let aniLoc = Animate.currentLocation animations (Animate.initPosition key)
  drawSprite (rCommanderSprites . cResources) aniLoc pos


def Grid.draw (grid : Grid): DrawM Unit := do
  for tile in grid.tiles.values do
    tile.draw
  let mCurrentTile <-
  mCurrentTile.draw

def Tile.draw (tile : Tile): DrawM Unit := do
  let mCurrentTile <- use (playVars . _pvSelectedTile)
  let points := tile.corners.map SDL.toSDLPoint
  let p := tile.center
  let {x, y, z, ..} := tile.coords
  let {showCoords, ..  : DrawData } ← get
  let oldColor <- getColorV4
  let color | maybe False (tile ==) mCurrentTile = Red
            | otherwise                          = Green
  setColor color
  drawLines points
  setColorV4 oldColor

  showCoords <- gets $ view (settings . sShowCoords)
  when showCoords $ do
    drawDigits (fromIntegral x) (T.p (-25) (-28) T.<+> p)
    drawDigits (fromIntegral y) (T.p (-5) (5) T.<+> p)
    drawDigits (fromIntegral z) (T.p (25) (-28) T.<+> p)
