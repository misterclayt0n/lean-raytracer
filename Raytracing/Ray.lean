import Raytracing.Vec

open Vec3

/--
A Ray in 3D space, defined by an origin point and a direction vector.
-/
structure Ray where
  orig : Point3
  dir  : Vec3
deriving Repr

namespace Ray

/--
Calculates a point in the ray at a given parameter `t`.
`P(t) = orig + t * dir`
-/

def pointAt (r : Ray) (t : Float) : Point3 :=
  r.orig + t * r.dir

end Ray

  
