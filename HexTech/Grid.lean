import Std
import HexTech.Utils
open Std
namespace HexTech



structure Point where
  x : Float
  y : Float
  deriving Repr, BEq, Hashable

def Point.add (p : Point) (q : Point) : Point := { x := p.x + q.x, y := p.y + q.y }

def Point.sub (p : Point) (q : Point) : Point := { x := p.x - q.x, y := p.y - q.y }

instance : Add Point := ⟨Point.add⟩
instance : Sub Point := ⟨Point.sub⟩

def AxialCoord := (Int × Int)
  deriving Repr, BEq

structure CubeCoord where
  x : Int
  y : Int
  z : Int
  h : x + y + z = 0
  deriving Repr, Hashable, Ord

instance : Repr CubeCoord where
  reprPrec := λ c _ => s!"({c.x}, {c.y}, {c.z})"

instance : BEq CubeCoord where
  beq | c1, c2 =>
        c1.x = c2.x ∧ c1.y = c2.y ∧ c1.z = c2.z

notation "(" x "," y "," z ")" h => (λ h => { x := x, y := y, z := z, h := h : CubeCoord })

open Int in
def CubeCoord.add : CubeCoord → CubeCoord → CubeCoord
| {x, y, z, h}, {x := sx, y := sy, z := sz, h := h2} => {
  x := (x + sx), y := (y + sy), z := (z + sz),
  h := (by
    rw [Int.add_left_comm, Int.add_left_comm, Int.add_right_comm,
      Int.add_right_comm, Int.add_left_comm, Int.add_assoc,
      ←Int.add_assoc, ←Int.add_assoc, ←Int.add_left_comm y,
      ←Int.add_assoc, ←Int.add_assoc,  ←Int.add_assoc,
      Int.add_right_comm y z, ←Int.add_comm x y, h, Int.zero_add, h2
    ]
  )
}

instance : Add CubeCoord where
  add := CubeCoord.add

structure Tile where
  coords : CubeCoord
  corners : Array Point
  center : Point
  deriving Repr, BEq, Hashable


structure GridArgs where
  radius : Nat
  position : Point
  size : Nat
  deriving Repr, BEq, Hashable

structure Grid where
  tiles : HashMap CubeCoord Tile
  args : GridArgs
  --deriving Repr, BEq


inductive Direction where
    | NorthEast
    | NorthWest
    | West
    | SouthWest
    | SouthEast
    | East
    deriving Repr, BEq

def Direction.all := [
  NorthEast,
  NorthWest,
  West,
  SouthWest,
  SouthEast,
  East
]

def Direction.shift : Direction -> CubeCoord
| NorthEast => { x := 1, y := 0, z := -1, h := (by simp; rw [←Int.sub_eq_add_neg, Int.sub_eq_zero]) }
| NorthWest => { x := 0, y := 1, z := -1, h := (by simp; rw [←Int.sub_eq_add_neg, Int.sub_eq_zero]) }
| West      => { x := -1,  y := 1, z := 0, h := (by simp; rw [Int.add_comm, ←Int.sub_eq_add_neg, Int.sub_eq_zero]) }
| SouthWest => { x := -1, y := 0, z := 1, h := (by simp; rw [Int.add_comm, ←Int.sub_eq_add_neg, Int.sub_eq_zero]) }
| SouthEast => { x := 0,  y := -1, z := 1, h := (by simp; rw [Int.add_comm, ←Int.sub_eq_add_neg, Int.sub_eq_zero]) }
| East      => { x := 1, y := -1, z := 0, h := (by simp; rw [←Int.sub_eq_add_neg, Int.sub_eq_zero]) }


def Tile.neighbours (t : Tile) : List CubeCoord :=
  Direction.all.map λ d => t.coords + d.shift


def Point.toTile (point : Point) (grid : Grid) : Option Tile :=
  let {x, y} := point - grid.args.position
  let size := grid.args.size.toFloat
  let q := Float.round $ (Float.sqrt 3 / 3 * x - 1 / 3 * y) / size
  let r := Float.round $ (2 / 3 * y) / size
  grid.tiles |> HashMap.findFirst
    (λ | {x,y,z, ..}, _ =>
        x = q.toUInt32.toNat ∧ y = (-q - r).toUInt32.toNat ∧ z = r.toUInt32.toNat
    )


def CubeCoord.toAxialCoords (c: CubeCoord) : AxialCoord := (c.x, c.z)

def AxialCoord.toCubeCoord : AxialCoord → CubeCoord
| (q, r) => {
  x := q, y := -q -r, z := r,
  h := by
    rw [
      ← Int.add_sub_assoc, ← Int.sub_eq_add_neg, Int.sub_add_cancel,
      Int.sub_eq_zero_of_eq
    ]
    eq_refl
}
open Float in
def calcTilePosition : GridArgs → (Int × Int) → Point
| { size, position, .. }, (q, r) =>
  let lineWidth := 1
  { x := round <| ofInt (size * r) * 3 / 2 - 2 * lineWidth,
    y := round <| ofInt size * (sqrt 3 * ofInt q + sqrt 3 / 2 * ofInt r) - 2 * lineWidth
  } + position



def pi := 3.141592653589793

open Float in
def pointyHexCorner : Point → Float → Float → Point
| { x, y }, size, i =>
  let angleRad := pi / 3.0 * i + pi / 6.0
  { x := round (x + size * cos (angleRad)),
    y := round (y + size * sin (angleRad))
  }

def makeHexagon (center : Point) (size : Float) : Array Point :=
  (0 .. 7) |>.map Float.ofInt |>.map (pointyHexCorner center size)

def hexagonGrid (args : GridArgs) : Grid :=
  let {radius, size, ..} := args
  let coords : Array CubeCoord :=
    ((-radius) .. radius) |>.flatMap λ x =>
      ((-radius) .. radius) |>.flatMap λ y =>
        ((-radius) .. radius) |>.map (λ z => (x,y,z))
    |>.filterMap (λ
    | (x,y,z) =>
      if h : x + y + z = 0 then
        some { x, y, z, h }
      else
        none
    )
  let tiles := coords.map (λ c =>
      let cp := calcTilePosition args (c.x, c.z)
      ( c,
      { coords := c
      , corners := makeHexagon cp (Float.ofNat size)
      , center := cp
      : Tile
      })
    )
    |>.toHashMap
  { tiles
  , args

  }
