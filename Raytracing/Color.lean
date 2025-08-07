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

def hit_sphere (center : Point3) (radius : Float) (ray : Ray) : Bool :=
  let oc           := center - ray.orig
  let a            := Vec3.dot ray.dir ray.dir
  let b            := -2.0 * Vec3.dot ray.dir oc
  let c            := Vec3.dot oc oc - radius^2
  let discriminant := b^2 - 4.0*a*c
  discriminant >= 0

def ray_color (r : Ray) : Color :=
  if hit_sphere ⟨0, 0, -1⟩ 0.5 r then ⟨1, 0, 0⟩ else
  
  let unit_dir := Vec3.normalize r.dir
  let a        := 0.5 * (unit_dir.y + 1.0)
  let white    := Color.mk 1.0 1.0 1.0
  let blue     := Color.mk 0.5 0.7 1.0

  (1.0 - a) * white + a * blue

end Color
  
