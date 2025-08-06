import Raytracing.Vec
import Raytracing.Ray

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

def ray_color (r : Ray) : Color :=
  let unit_dir := Vec3.normalize r.dir
  let a        := 0.5 * (unit_dir.y + 1.0)
  let white    := Color.mk 1.0 1.0 1.0
  let blue     := Color.mk 0.5 0.7 1.0

  (1.0 - a) * white + a * blue

end Color
  
