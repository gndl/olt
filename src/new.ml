open Gradient
open Motif
open Image
open GraphicImage
open Canvas
open GraphicCanvas
open Usual

let linearGradient angle =
  if angle > 45 && angle < 135 then (new Gradient.c Vertical)
  else (new Gradient.c Horizontal)


let motif = new Motif.c

let image ?(w=1) ?(h=1) () = (new GraphicImage.c w h :> Image.c)

let imageOfColorMatrix cm ?(w=1) ?(h=1) () =
  (new GraphicImage.c ~colorMatrix:cm w h :> Image.c)


(*let newCanvas ?(x=0) ?(y=0) ?(w=0) ?(h=0) =*)
let window w h title = (new GraphicWindow.c w h title :> Window.c)

let canvas ?(x=0) ?(y=0) ?(w=0) ?(h=0) ?(area) () = (
  new GraphicCanvas.c x y w h
    (match area with Some a -> a | None -> new Area.c x y w h)
  :> Canvas.c)


let graphicRelease = Graphics.synchronize

let checkWindowSize windowWidth windowHeight =
  let w = Graphics.size_x() and h = Graphics.size_y()
  in
  if windowWidth > w || windowHeight > h then
    (
      Graphics.resize_window (maxi w windowWidth) (maxi h windowHeight)
    )


