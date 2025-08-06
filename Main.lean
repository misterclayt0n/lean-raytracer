import Raytracing

def main : IO Unit := do
  let image_width  := 256
  let image_height := 256

  IO.println s!"P3"
  IO.println s!"{image_width} {image_height}"
  IO.println s!"255"

  for j in [0:image_height] do
    let progress := (j.toFloat / (image_height - 1).toFloat) * 100.0
    IO.eprint s!"\x1b[2K\x1b[0GRendering: {progress}%"  
    
    for i in [0:image_width] do
      let r := i.toFloat / (image_width  - 1).toFloat
      let g := j.toFloat / (image_height - 1).toFloat
      let b := 0.0
      IO.println (Color.toPPMLine (Color.mk r g b))
      
  IO.eprintln s!"\x1b[2K\x1b[0GDone."
