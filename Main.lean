inductive Pos : Type where
  | one  : Pos
  | succ : Pos -> Pos

def seven : Pos :=
  Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ Pos.one)))))

class Plus (T : Type) where
  plus : T -> T -> T

instance : Plus Nat where
  plus := Nat.add

open Plus (plus)

def addPos : Pos -> Pos -> Pos
  | Pos.one, k    => Pos.succ k 
  | Pos.succ n, k => Pos.succ (addPos n k)

instance : Add Pos where
  add := addPos

def posToString (atTop : Bool) (p : Pos) : String :=
  let paren s : String := if atTop then s else "(" ++ s ++ ")"
  match p with
  | Pos.one    => "Pos.one"
  | Pos.succ n => paren s!"Pos.succ {posToString false n}"

def Pos.toNat : Pos -> Nat
  | Pos.one    => 1 
  | Pos.succ n => n.toNat + 1 
  
instance : ToString Pos where
  toString x := toString (x.toNat)

def Pos.mul : Pos -> Pos -> Pos
  | Pos.one, k    => k
  | Pos.succ n, k => n.mul k + k

instance : Mul Pos where
  mul := Pos.mul

instance : One Pos where
  one := Pos.one

instance : OfNat Pos (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat -> Pos
      | 0     => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n

def addNatPos : Nat → Pos → Pos
  | 0, p     => p
  | n + 1, p => Pos.succ (addNatPos n p)

def addPosNat : Pos → Nat → Pos
  | p, 0     => p
  | p, n + 1 => Pos.succ (addPosNat p n)

instance : HAdd Nat Pos Pos where
  hAdd := addNatPos

instance : HAdd Pos Nat Pos where
  hAdd := addPosNat

def whatever : Array String := #["yes", "daddy"]

structure NonEmptyList (T : Type) : Type where
  head : T
  tail : List T 

def positions : NonEmptyList String := {
  head := "de quatro",
  tail := [
    "de ladinho",
    "sentada violenta"
  ]
}

def NonEmptyList.get? : NonEmptyList T -> Nat -> Option T
  | xs, 0                              => some xs.head
  | {head := _, tail := []}, _ + 1     => none
  | {head := _, tail := h :: t}, n + 1 => get? {head := h, tail := t} n

abbrev NonEmptyList.inBounds (xs : NonEmptyList T) (i : Nat) : Prop :=
  i <= xs.tail.length

theorem atLeastThree : positions.inBounds 2 := by decide

def NonEmptyList.get (xs : NonEmptyList T) (i : Nat) (ok : xs.inBounds i) : T :=
  match i with
  | 0     => xs.head
  | n + 1 => xs.tail[n]

#eval positions.get 2 atLeastThree

instance : GetElem (NonEmptyList T) Nat T NonEmptyList.inBounds where
  getElem := NonEmptyList.get

instance : GetElem (List T) Pos T (fun list n => list.length > n.toNat) where
  getElem (xs : List T) (i : Pos) ok := xs[i.toNat]

#eval positions[2]

def Pos.comp : Pos -> Pos -> Ordering
  | Pos.one, Pos.one       => Ordering.eq
  | Pos.one, Pos.succ _    => Ordering.lt
  | Pos.succ _, Pos.one    => Ordering.gt
  | Pos.succ n, Pos.succ k => comp n k

deriving instance BEq, Hashable for Pos

instance : Ord Pos where
  compare := Pos.comp

instance : Functor NonEmptyList where
  map f xs := { head := f xs.head, tail := f <$> xs.tail }

instance : Coe Pos Nat where
  coe x := x.toNat

instance : Coe (NonEmptyList T) (List T) where
  coe | { head := x, tail := xs } => x :: xs

#check [1, 2, 3, 4].drop (2 : Pos)

def first (xs : List T) : Option T :=
  xs[0]?

def firstThird (xs : List T) : Option (Prod T T) :=
  match xs[0]? with
  | none       => none
  | some first =>
    match xs[2]? with
    | none       => none
    | some third => some (first, third)

def aLot [Monad m] (lookup : List T -> Nat -> m T) (xs : List T) : m (T × T × T × T ) :=
  lookup xs 0 >>= fun first  =>
  lookup xs 2 >>= fun third  =>
  lookup xs 4 >>= fun fifth  =>
  lookup xs 6>>= fun seventh =>
  pure (first, third, fifth, seventh)


def slowMammals : List String :=
  ["Three-toed sloth", "Slow loris"]

def fastBirds : List String := [
  "Peregrine falcon",
  "Saker falcon",
  "Golden eagle",
  "Gray-headed albatross",
  "Spur-winged goose",
  "Swift",
  "Anna's hummingbird"
]  

#eval aLot (fun xs i => xs[i]?) slowMammals

inductive Expr (op : Type) where
  | const : Int -> Expr op
  | prim  : op -> Expr op -> Expr op -> Expr op

inductive Arith where
  | plus
  | minus
  | times
  | div

open Expr in 
open Arith in
def twoPlusThree : Expr Arith := prim plus (const 2) (const 3)

def hello : Nat -> String := fun n => toString n ++ "!"
def g : Nat -> Nat := fun n => n * 2

#eval Option.map hello (some 5)
#eval Option.map hello none
#eval Option.map g (some 10)

#eval g 10
#eval List.map g [1, 2, 3, 4]
#eval List.map hello [1, 2, 3, 4]

def safeDivBy2 (n : Nat) : Option Nat :=
  if n % 2 == 0 then some (n / 2) else none

def safeIncrement (n : Nat) : Option Nat :=
  if n < 10 then some (n + 1) else none

def processNumber (n : Nat) : Option Nat := do
  let m1 <- safeDivBy2 n
  let m2 <- safeIncrement m1
  return m2

#eval (pure 10 : Option Nat)
#eval (some 5 >>= safeDivBy2) >>= safeIncrement
#eval processNumber 4


inductive Validation (Errors : Type) (T : Type) where
  | ok (val : T)
  | error (errs: List Errors)
deriving Repr

instance : Applicative (Validation (List String)) where
  pure val := Validation.ok val
  seq f_val a_val :=
    match f_val, a_val with
    | Validation.ok f,      Validation.ok a     => Validation.ok (f a)
    | Validation.ok f,      Validation.error es => Validation.error es
    | Validation.error ef,  Validation.ok a     => Validation.error ef
    | Validation.error ef,  Validation.error ea => Validation.error (ef ++ ea) -- IMPORTANT: Combines errors!
