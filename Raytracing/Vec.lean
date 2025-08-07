structure Vec3 where
  x : Float
  y : Float
  z : Float
deriving Repr

namespace Vec3

def zero : Vec3 := ⟨0.0, 0.0, 0.0⟩

instance : Neg Vec3 where
  neg v := ⟨-v.x, -v.y, -v.z⟩

-- 
-- Boilerplate mostly.
-- 

def addAssign (v1 : Vec3) (v2 : Vec3) : Vec3 :=
  ⟨v1.x + v2.x, v1.y + v2.y, v1.z + v2.z⟩

def addFloatAssign (v1 : Vec3) (f : Float) : Vec3 :=
  ⟨v1.x + f, v1.y + f, v1.z + f⟩

def subAssign (v1 : Vec3) (v2 : Vec3) : Vec3 :=
  ⟨v1.x - v2.x, v1.y - v2.y, v1.z - v2.z⟩
  
def mulAssign (v1 : Vec3) (v2 : Vec3) : Vec3 :=
  ⟨v1.x * v2.x, v1.y * v2.y, v1.z * v2.z⟩

def mulAssignScalar (v : Vec3) (t : Float) : Vec3 :=
  ⟨v.x * t, v.y * t, v.z * t⟩
 
def divAssignScalar (v : Vec3) (t : Float) : Vec3 :=
  v.mulAssignScalar (1.0 / t)

def lengthSq (v : Vec3) : Float :=
  v.x^2 + v.y^2 + v.z^2

def length (v : Vec3) : Float :=
  Float.sqrt (v.lengthSq) -- Kinda cancels out.

-- 
-- The real deal (operator overloading)
-- 

instance : Add Vec3 where
  add := addAssign

instance : HAdd Vec3 Float Vec3 where
  hAdd := addFloatAssign

instance : HAdd Vec3 Nat Vec3 where
  hAdd v n := addFloatAssign v n.toFloat

instance : Sub Vec3 where
  sub := subAssign

-- Element wise multiplication (Hadamard product)
instance : Mul Vec3 where
  mul := mulAssign

-- (Vec3 * Float)
instance : HMul Vec3 Float Vec3 where
  hMul := mulAssignScalar

-- (Float * Vec3)
instance : HMul Float Vec3 Vec3 where
  hMul t v := mulAssignScalar v t 

instance : HMul Nat Vec3 Vec3 where
  hMul t v := mulAssignScalar v t.toFloat

instance : HMul Vec3 Nat Vec3 where
  hMul v t := mulAssignScalar v t.toFloat 
  
instance : HDiv Vec3 Float Vec3 where
  hDiv := divAssignScalar

def dot (v1 v2 : Vec3) : Float :=
  v1.x * v2.x + v1.y * v2.y + v1.z * v2.z

def cross (v1 v2 : Vec3) : Vec3 :=
  ⟨v1.y * v2.z - v1.z * v2.y,
   v1.z * v2.x - v1.x * v2.z,
   v1.x * v2.y - v1.y * v2.x⟩

-- Unit vector.
def normalize (v : Vec3) : Vec3 :=
  v / v.length

instance : ToString Vec3 where
  toString v := s!"{v.x} {v.y} {v.z}"

end Vec3

-- Alias for Point3 for geometric clarity.
abbrev Point3 := Vec3
