import Raytracing.Vec

open Vec3

abbrev Color := Vec3

namespace Color

def mk (r g b : Float) : Color := ⟨r, g, b⟩

def black : Color := mk 0.0 0.0 0.0
def white : Color := mk 1.0 1.0 1.0

def clamp (x : Float) : Float :=
  if x < 0 then 0 else if x > 1 then 1 else x

def map (f : Float -> Float) (c : Color) : Color :=
  ⟨f c.x, f c.y, f c.z⟩

/--
Convert to byte triplet
-/
def toBytes (c : Color) : UInt8 × UInt8 × UInt8 :=
  let c' := map clamp c
  let s (x : Float) : UInt8 := (255.999 * x).toUInt8
  (s c'.x, s c'.y, s c'.z)  

def toPPMLine (c : Color) : String :=
  let (r, g, b) := toBytes c
  s!"{r} {g} {b}"
  
end Color
  
