import Std
open Std

instance : Hashable Float where
  hash f := f.toUInt64

instance : HMul Nat Int Int where
  hMul n i := n * i

def Int.range (start stop : Int) : Array Int :=
  let rec go
    | 0, _, arr => arr
    | n+1, start, arr => go n (start + 1) (arr.push start)
  let n := (stop - start).toNat
  go n start (Array.mkEmpty n)

notation start ".." stop => Int.range start stop

def Array.flatMap (l : Array α) (f : α → Array β) : Array β :=
  l.map f |>.flatten

def HashMap.findFirst {K V : Type} [BEq K] [BEq V] [Hashable K] [Hashable V]
  (f : K → V → Bool) (self : HashMap K V) : Option V :=
  self.fold (λ d k v => if d.isNone ∧ f k v then some v else none) none

def HashMap.fromArray {K V : Type} [BEq K] [BEq V] [Hashable K] [Hashable V]
  (arr : Array (K × V)) : HashMap K V :=
  arr.foldl (λ map (k, v) => map.insert k v) <| mkHashMap (capacity := arr.size)

def Array.toHashMap {K V : Type} [BEq K] [BEq V] [Hashable K] [Hashable V]
  (arr : Array (K × V)) : HashMap K V := HashMap.fromArray arr

-- syntax (priority := high) "{" (term "=>" term),+ "}" : term

-- /- Declares two expansions/syntax transformers -/
-- macro_rules
--   | `({$x => $y}) => `(#[($x, $y)].toHashMap)
--   | `({$x => $y, ${xs:term => ys:term},*}) => `($x {$xs,*})
