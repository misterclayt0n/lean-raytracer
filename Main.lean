import Raytracing

def image_width     : Nat    := 256
def aspect_ratio    : Float  := 16 / 9
def image_height    : Nat    := (image_width.toFloat / aspect_ratio).floor.toUInt16.toNat
def focal_length    : Float  := 1.0
def viewport_height : Float  := 2.0
def viewport_width  : Float  := viewport_height * (image_width/image_height).toFloat
def camera_center   : Point3 := ⟨0.0, 0.0, 0.0⟩

-- Calculate the vectors across the horizontal and down the vertical viewport edges.
def viewport_u : Vec3 := ⟨viewport_width, 0.0, 0.0⟩
def viewport_v : Vec3 :=  ⟨0.0, -viewport_height, 0.0⟩

-- Calculate the horizontal and vertical delta vectors from pixel to pixel.
def pixel_delta_u : Vec3 := viewport_u / image_width.toFloat
def pixel_delta_v : Vec3 := viewport_v / image_height.toFloat

-- Calculate the location of the upper left pixel.
def viewport_upper_left : Vec3 := camera_center - ⟨0.0, 0.0, focal_length⟩ - viewport_u/2.0 - viewport_v/2.0
def pixel00_loc         : Vec3 := viewport_upper_left + (0.5 : Float) * (pixel_delta_u + pixel_delta_v)
  
def main : IO Unit := do
  IO.println s!"P3"
  IO.println s!"{image_width} {image_height}"
  IO.println s!"255"

  for j in [0:image_height] do
    let progress := (j.toFloat / (image_height - 1).toFloat) * 100.0
    IO.eprint s!"\x1b[2K\x1b[0GRendering: {progress}%"  
    
    for i in [0:image_width] do
      let pixel_center  := pixel00_loc + (pixel_delta_u * i) + (j * pixel_delta_v)
      let ray_direction := pixel_center - camera_center
      let r : Ray       := ⟨camera_center, ray_direction⟩ 
      let pixel_color   := Color.ray_color r
      
      IO.println (Color.toPPMLine pixel_color)
      
  IO.eprintln s!"\x1b[2K\x1b[0GDone."
