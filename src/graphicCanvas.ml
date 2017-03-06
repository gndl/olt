open Usual
open Matrix3D
open Style
open Motif
open Image
open GraphicImage
open Gradient
open Canvas
open Graphics

let getColorOfString s =
  let hexaToBin c =
    match c with
		| '0'..'9' -> int_of_char c - int_of_char '0'
    | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
    | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
    | _ -> 0
  in
    match String.length s with
    | 4 | 5 -> rgb (hexaToBin s.[1] * 16) (hexaToBin s.[2] * 16) (hexaToBin s.[3] * 16)
    | 7 | 9 -> rgb(hexaToBin s.[1] * 16 + hexaToBin s.[2]) (hexaToBin s.[3] * 16 + hexaToBin s.[4]) (hexaToBin s.[5] * 16 + hexaToBin s.[6]) 
    | _ -> rgb 0 0 0
;;

let fillHorizontalGradientPolygon pts grad =
	let sOf r = r lsl 10 and rOf s = s asr 10 in
	let tlY = snd pts.(3) and trY = snd pts.(2)
	and blX = fst pts.(0) and blY = snd pts.(0) and brX = fst pts.(1) and brY = snd pts.(1)
	in
	let n = brX - blX + 1
	in
	if n > 0 then (
		let colors = grad#getColors n
		and sby = ref(sOf blY) and sty = ref(sOf tlY) and dby = sOf(brY - blY) / n and dty = sOf(trY - tlY) / n
		in
		for x = blX to brX do
			let c = colors.(x - blX) in
			Graphics.set_color (rgb c.red c.green c.blue);
			Graphics.moveto x (rOf !sby);
			Graphics.lineto x (rOf !sty);
			sby := !sby + dby;
			sty := !sty + dty;
		done
	)
	 
	
let fillVerticalGradientPolygon pts grad =
	let sOf r = r lsl 10 and rOf s = s asr 10 in
	let tlX = fst pts.(3) and tlY = snd pts.(3) and trX = fst pts.(2)
	and blX = fst pts.(0) and blY = snd pts.(0) and brX = fst pts.(1)
	in
	let n = tlY - blY + 1
	in
	let colors = grad#getColors n
	and slx = ref(sOf blX) and srx = ref(sOf brX) and dlx = sOf(tlX - blX) / n and drx = sOf(trX - brX) / n
	in
	for y = blY to tlY do
		let c = colors.(y - blY) in
		Graphics.set_color (rgb c.red c.green c.blue);
		Graphics.moveto (rOf !slx) y;
		Graphics.lineto (rOf !srx) y;
		slx := !slx + dlx;
		srx := !srx + drx;
	done

let fillRadialGradientPolygon pts grad = () (* TODO *)

let fillGradientPolygon pts grad =
	match grad#style with
		| Vertical -> fillVerticalGradientPolygon pts grad
		| Horizontal -> fillHorizontalGradientPolygon pts grad
		| Radial -> fillRadialGradientPolygon pts grad
		 
(*
let fillGradientPolygon pts grad =
	let tlX = fst pts.(0) and tlY = snd pts.(0) and trX = fst pts.(1) and trY = snd pts.(1)
	and blX = fst pts.(3) and blY = snd pts.(3) and brX = fst pts.(2) and brY = snd pts.(2)
	in
	let topAB = getAB tlX tlY trX trY	and botAB = getAB blX blY brX brY
	in
	let ta = fst topAB and tb = snd topAB and ba = fst botAB and bb = snd botAB
	and n = trX - tlX + 1
	in
	let colors = grad#getColors n
	in
	for x = blX to brX do
		let c = colors.(x - blX) in
		Graphics.set_color (rgb c.red c.green c.blue);
		let by = ba * x + bb and ty = ta * x + tb
		in
		Graphics.moveto x by;
		Graphics.lineto x ty;
	done
	 
		let lx = ref max_int and lp = ref 0 and lastPt = Array.length pts - 1
	in
	for i = 0 to lastPt do
		let x = fst pts.(i)
		in
		if x < !lx then (lx := x; lp := i;)
	done;

	let prevPt = lp - 1 and nextPt = lp + 1
	in
	
	if lp = 0 then prevPt := lastPt
	else if lp = lastPt then nextPt := 0
	
	let bl = ref {x = fst pts.(lp); y = snd pts.(lp)}
	and tl = ref {x = fst pts.(prevPt); y = snd pts.(prevPt)}
	and tr = ref {x = fst pts.(prevPt); y = snd pts.(prevPt)}
	in
	if 
*)

 
class c xo yo wo ho area =
	object(self:'s)
	inherit Canvas.c area

  val mutable mMatrix = new cSimpleMatrix3D
  val mutable mMatrixStack = []
  val mutable mStateStack = []
(*  val mPath = Queue.create *)
(*  val mutable mPath = ([||]:tStep array)*)
(*  val mutable mPath = [||] *)
(*	val mMotif = new cFmotif*)
  (*val mutable element = surface;*)

  (* Canvas context properties *)
	val mutable mBackground = Style.backgroundFill

	val mutable mProperties = {
(*		fillStyle = PlainRgb 0xc0c0c0;*)
		fillStyle = Color 0x00FF00;
		lineCap = Butt;
		lineJoin = Miter;
		lineWidth = 0;
		miterLimit = 10.0;
		shadowBlur = 0;
		shadowColor = 0;
		shadowOffsetX = 0;
		shadowOffsetY = 0;
		strokeStyle = "#000";
		globalAlpha = 1.0;
    font = "18px sans-serif";
    textAlign = Start;
    textBaseline = Alphabetic;
		}

  method setBackground    v = mBackground <- v
  method setFillStyle     v = mProperties.fillStyle <- v; self#processFillStyle v
  method setLineCap       v = mProperties.lineCap <- v; self#processStyle mProperties
  method setLineJoin      v = mProperties.lineJoin <- v; self#processStyle mProperties
  method setLineWidth     v = mProperties.lineWidth <- v; self#processLineWidth v
  method setMiterLimit    v = mProperties.miterLimit <- v; self#processStyle mProperties
  method setShadowBlur    v = mProperties.shadowBlur <- v; self#processStyle mProperties
  method setShadowColor   v = mProperties.shadowColor <- v; self#processStyle mProperties
  method setShadowOffsetX v = mProperties.shadowOffsetX <- v; self#processStyle mProperties
  method setShadowOffsetY v = mProperties.shadowOffsetY <- v; self#processStyle mProperties
  method setStrokeStyle   v = mProperties.strokeStyle <- v; self#processStyle mProperties
  method setGlobalAlpha   v = mProperties.globalAlpha <- v; self#processStyle mProperties
  method setFont          v = mProperties.font <- v; self#processFont v
  method setTextAlign     v = mProperties.textAlign <- v; self#processStyle mProperties
  method setTextBaseline  v = mProperties.textBaseline <- v; self#processStyle mProperties
	
	method activeStyle() = self#processStyle mProperties

  method setProperties    v = mProperties <- v; self#processStyle mProperties
  method getProperties()    = mProperties

  method processFillStyle fillStyle =
    match fillStyle with
      | PlainRgb c -> set_color c
      | PlainRgba v -> set_color (rgb v.red v.green v.blue)
      | PlainRgbaString s -> set_color (getColorOfString s)
      | Gradient g -> let c = (g#getColors 1).(0) in set_color (rgb c.red c.green c.blue); (*trace("red "^soi c.red^", green "^soi c.green^", blue "^soi c.blue)*)
      | Image img -> ()
      | Color c -> set_color c

  method processLineWidth lineWidth = Graphics.set_line_width lineWidth;

  method processStyle properties =
    self#processFillStyle properties.fillStyle;
    self#processLineWidth properties.lineWidth;

  method processFont font = Graphics.set_font font;

  method getImage() =
		let xo = mArea#x and yo = mArea#y and w = mArea#w and h = mArea#h in
    ((new GraphicImage.c ~subScreenshot:true ~xo ~yo w h) :> Image.c)

  method getSubImage x y w h =
		let xo = mArea#x and yo = mArea#y in
    ((new GraphicImage.c ~subScreenshot:true ~xo:(x + xo) ~yo:(y + yo) w h) :> Image.c)
(*    img#subScreenshot (x + xo) (y + yo) w h; 
    (img :> Image.c)*)
    
  method setImage img =
		let wo = mArea#w and ho = mArea#h in
		self#setSubImage img 0 0 wo ho

  method setSubImage img x y w h =
		let xo = mArea#x and yo = mArea#y in
		img#subScreenshot (x + xo) (y + yo) w h
  
(*  method drawImage img ?(dx = 0) ?(dy = 0) ?(dw = 0) ?(dh = 0) ?(sx = 0) ?(sy = 0) ?(sw = 0) ?(sh = 0) = *)
  method drawImage img x y =
		let xo = mArea#x and yo = mArea#y in
		img#draw (x + xo) (y + yo)
(*
  method beginPath() = mMotif#beginPath()
  method setPathFillStyle fs = mMotif#setFillStyle fs
  method draw() = self#drawMotif mMotif 0 0
  method closePath() = mMotif#closePath()
*)
  method moveTo x y = Graphics.moveto (x + mArea#x) (y + mArea#y)
  method lineTo x y = Graphics.lineto (x + mArea#x) (y + mArea#y)
  method lines points =
		let xo = mArea#x and yo = mArea#y in
		Graphics.draw_poly_line (Array.map (fun pt -> fst pt + xo, snd pt + yo) points)

  method fillRect x y w h =
		let ax = x + mArea#x and ay = y + mArea#y in
		match mProperties.fillStyle with
		| Gradient g -> fillGradientPolygon [|(ax, ay); (ax + w, ay); (ax + w, ay + h); (ax, ay + h)|] g
  	| _ -> Graphics.fill_rect ax ay w h

  method strokeRect x y w h = Graphics.draw_rect (x + mArea#x) (y + mArea#y) w h

  method polygon points =
		let xo = mArea#x and yo = mArea#y in
		match mProperties.fillStyle with
		| Gradient g -> fillGradientPolygon (Array.map (function pt -> fst pt + xo, snd pt + yo) points) g
		| Color v -> Graphics.draw_poly (Array.map (function pt -> fst pt + xo, snd pt + yo) points)
		| _ -> Graphics.fill_poly (Array.map (function pt -> fst pt + xo, snd pt + yo) points)

  method bezierCurveTo cp1x cp1y cp2x cp2y x y =
		let xo = mArea#x and yo = mArea#y in
		Graphics.curveto (cp1x + xo, cp1y + yo) (cp2x + xo, cp2y + yo) (x + xo, y + yo)

  method quadraticCurveTo cpx cpy x y = self#bezierCurveTo cpx cpy cpx cpy x y

  method bezierCurve sx sy cp1x cp1y cp2x cp2y ex ey =
		let xo = mArea#x and yo = mArea#y in
		Graphics.moveto (sx + xo) (sy + yo);
		self#bezierCurveTo  cp1x cp1y cp2x cp2y ex ey

  method arc x y radiusX radiusY startAngle endAngle =
		let xo = mArea#x and yo = mArea#y in
		match mProperties.fillStyle with
		| Gradient g -> Graphics.fill_arc (x + xo) (y + yo) radiusX radiusY startAngle endAngle
		| Color v -> Graphics.draw_arc (x + xo) (y + yo) radiusX radiusY startAngle endAngle
  	| _ -> Graphics.fill_arc (x + xo) (y + yo) radiusX radiusY startAngle endAngle

	method fillText text x y ?maxWidth () =
		Graphics.moveto (x + mArea#x) (y + mArea#y); Graphics.draw_string text

	method strokeText text x y ?maxWidth () =
		Graphics.moveto (x + mArea#x) (y + mArea#y); Graphics.draw_string text

	method measureText text = let (w, h) = Graphics.text_size text in {tWidth = w}

  method drawMotif motif x y =
		let xo = mArea#x and yo = mArea#y in
    let ax = xo + x and ay = yo + y and fillStyle = ref mProperties.fillStyle
    in
    let ptConv pt = (fst pt + ax, snd pt + ay)
    in
    let treatStep step =
			match step with
      | MoveTo p -> Graphics.moveto (p.x + ax) (p.y + ay)
      | Points pts -> Graphics.plots (Array.map ptConv pts)
      | Lines pts -> Graphics.draw_poly_line (Array.map ptConv pts)
      | FillRect (x, y, w, h) -> (
				let ax = x + xo and ay = y + yo in
				match !fillStyle with
				| Gradient g -> fillGradientPolygon [|(ax, ay); (ax + w, ay); (ax + w, ay + h); (ax, ay + h)|] g
		  	| _ -> Graphics.fill_rect ax ay w h	)
			| StrokeRect (x, y, w, h) -> Graphics.draw_rect (x + xo) (y + yo) w h
      | Polygon pts -> (
				match !fillStyle with
				| Gradient g -> fillGradientPolygon (Array.map ptConv pts) g
				| Color v -> Graphics.draw_poly (Array.map ptConv pts)
      	| _ -> Graphics.fill_poly (Array.map ptConv pts) )
			| BezierCurve bc -> Graphics.moveto (bc.bcsx + ax) (bc.bcsy + ay);
          Graphics.curveto (bc.bccp1x + ax, bc.bccp1y + ay) (bc.bccp2x + ax, bc.bccp2y + ay) (bc.bcex + ax, bc.bcey + ay)
			| Arc a -> (
				match !fillStyle with
				| Gradient g -> Graphics.fill_arc (a.cx + ax) (a.cy + ay) a.rx a.ry a.a1 a.a2
				| Color v -> Graphics.draw_arc (a.cx + ax) (a.cy + ay) a.rx a.ry a.a1 a.a2
      	| _ -> Graphics.fill_arc (a.cx + ax) (a.cy + ay) a.rx a.ry a.a1 a.a2 )
			| FillText t -> Graphics.moveto (t.textx + ax) (t.texty + ay); Graphics.draw_string t.text
			| StrokeText t -> Graphics.moveto (t.textx + ax) (t.texty + ay); Graphics.draw_string t.text
      | ImageAt imgHandler -> Graphics.draw_image (Graphics.make_image imgHandler.img#getColorMatrix) (imgHandler.imgx + ax) (imgHandler.imgy + ay)
      | SetFillStyle fs -> self#processFillStyle fs; fillStyle := fs
      | SetLineCap       a -> ()
      | SetLineJoin      a -> ()
      | SetLineWidth     a -> self#processLineWidth a
      | SetMiterLimit    a -> ()
      | SetShadowBlur    a -> ()
      | SetShadowColor   a -> ()
      | SetShadowOffsetX a -> ()
      | SetShadowOffsetY a -> ()
      | SetStrokeStyle   a -> ()
      | SetGlobalAlpha   a -> ()
      | SetFont          a -> self#processFont a
      | SetTextAlign     a -> ()
      | SetTextBaseline  a -> ()
      | SetProperties    a -> self#processStyle a
(*      | Close -> ()*)
    in
      Array.iter treatStep motif#getPath;
          
      self#processStyle mProperties


  method drawMotifScaled motif lX bY percent =
(*trace("lX "^soi lX^", lY "^soi lY^", rX "^soi rX^", rY "^soi rY^", lH "^soi lH^", rH "^soi rH);*)
		let xo = mArea#x and yo = mArea#y in
    let scaledX x = xo + lX + (x * percent) / 100 and
    scaledY y = yo + bY + (y * percent) / 100 and scaled v = (v * percent) / 100 
    in
    let scaledXY x y = (scaledX x, scaledY y) and ptConv pt = (scaledX(fst pt), scaledY(snd pt))
    and fillStyle = ref mProperties.fillStyle and fill = ref (isFillOn mProperties.fillStyle)
    in
    let treatStep step = match step with
      | MoveTo p -> Graphics.moveto (scaledX p.x) (scaledY p.y)
      | Points pts -> Graphics.plots (Array.map ptConv pts)
      | Lines pts -> Graphics.draw_poly_line (Array.map ptConv pts)
      | FillRect (x, y, w, h) -> (
				let ax = scaledX x and ay = scaledY y and sw = scaled w and sh = scaled h in 
				match !fillStyle with
				| Gradient g -> fillGradientPolygon [|(ax, ay); (ax + sw, ay); (ax + sw, ay + sh); (ax, ay + sh)|] g
		  	| _ -> Graphics.fill_rect ax ay sw sh	)
			| StrokeRect (x, y, w, h) -> Graphics.draw_rect(scaledX x)(scaledY y)(scaled w)(scaled h)
      | Polygon pts -> if !fill then
                         Graphics.fill_poly (Array.map ptConv pts)
                       else
                         Graphics.draw_poly (Array.map ptConv pts)
      | BezierCurve bc -> Graphics.moveto (scaledX bc.bcsx) (scaledY bc.bcsy);
          Graphics.curveto (scaledXY bc.bccp1x bc.bccp1y) (scaledXY bc.bccp2x bc.bccp2y) (scaledXY bc.bcex bc.bcey)
      | Arc a ->
          let sx = scaledX a.cx and sy = scaledY a.cy and srx = scaled a.rx and sry = scaled a.ry
          in (*trace("cx "^soi a.cx^", cy "^soi a.cy^", rx "^soi a.rx^", ry "^soi a.ry);trace("sx "^soi sx^", sy "^soi sy^", srx "^soi srx^", sry "^soi sry);*)
            if !fill then
              Graphics.fill_arc sx sy srx sry a.a1 a.a2
            else
              Graphics.draw_arc sx sy srx sry a.a1 a.a2
			| FillText t -> Graphics.moveto (scaledX t.textx) (scaledY t.texty); Graphics.draw_string t.text
			| StrokeText t -> Graphics.moveto (scaledX t.textx) (scaledY t.texty); Graphics.draw_string t.text
      | ImageAt imgHdl ->
          let slx = scaledX imgHdl.imgx and sly = scaledY imgHdl.imgy
          in
          imgHdl.img#drawScaled slx sly percent
      | SetFillStyle fs -> self#processFillStyle fs; fillStyle := fs; fill := isFillOn(fs)
      | SetLineCap       a -> ()
      | SetLineJoin      a -> ()
      | SetLineWidth     a -> self#processLineWidth a
      | SetMiterLimit    a -> ()
      | SetShadowBlur    a -> ()
      | SetShadowColor   a -> ()
      | SetShadowOffsetX a -> ()
      | SetShadowOffsetY a -> ()
      | SetStrokeStyle   a -> () 
      | SetGlobalAlpha   a -> ()
      | SetFont          a -> self#processFont a
      | SetTextAlign     a -> ()
      | SetTextBaseline  a -> ()
      | SetProperties    a -> self#processStyle a
(*      | Close -> ()*)
    in
      Array.iter treatStep motif#getPath;
            
      self#processStyle mProperties


  method drawMotifShaped motif lX lY rX rY lH rH =
(*trace("lX "^soi lX^", lY "^soi lY^", rX "^soi rX^", rY "^soi rY^", lH "^soi lH^", rH "^soi rH);*)
		let xo = mArea#x and yo = mArea#y in
    let w = rX - lX and wi = motif#w and hi = motif#h
    in
    let shapedX x = xo + lX + (x * w) / wi and
    shapedY x y = (y * (lH + ((rH - lH) * x) / wi)) / hi + lY + yo + ((rY - lY) * x) / wi
    in
    let shapedXY x y = (shapedX x, shapedY x y) in let ptConv (x, y) = shapedXY x y
    and fillStyle = ref mProperties.fillStyle and fill = ref (isFillOn mProperties.fillStyle)
    in
    let treatStep step = match step with
      | MoveTo p -> Graphics.moveto (shapedX p.x) (shapedY p.x p.y)
      | Points pts -> Graphics.plots (Array.map ptConv pts)
      | Lines pts -> Graphics.draw_poly_line (Array.map ptConv pts)
      | FillRect (x, y, w, h) -> ( let rx = x + w and ty = y + h in
				match !fillStyle with
				| Gradient g -> fillGradientPolygon [|(shapedXY x y);(shapedXY rx y);(shapedXY rx ty);(shapedXY x ty)|] g
		  	| _ -> Graphics.fill_poly[|(shapedXY x y); (shapedXY rx y); (shapedXY rx ty); (shapedXY x ty)|] )
			| StrokeRect (x, y, w, h) -> let rx = x + w and ty = y + h in
				Graphics.draw_poly[|(shapedXY x y); (shapedXY rx y); (shapedXY rx ty); (shapedXY x ty)|]
      | Polygon pts -> if !fill then
                         Graphics.fill_poly (Array.map ptConv pts)
                       else
                         Graphics.draw_poly (Array.map ptConv pts)
      | BezierCurve bc -> Graphics.moveto (shapedX bc.bcsx) (shapedY bc.bcsx bc.bcsy);
          Graphics.curveto (shapedXY bc.bccp1x bc.bccp1y) (shapedXY bc.bccp2x bc.bccp2y) (shapedXY bc.bcex bc.bcey)
      | Arc a ->
          let sx = shapedX a.cx and sy = shapedY a.cx a.cy and srx = (shapedX(a.cx + a.rx / 2) - shapedX(a.cx - a.rx / 2))
          in let sry = (shapedY a.cx (a.cy + a.ry)) - sy
             in (*trace("cx "^soi a.cx^", cy "^soi a.cy^", rx "^soi a.rx^", ry "^soi a.ry);trace("sx "^soi sx^", sy "^soi sy^", srx "^soi srx^", sry "^soi sry);*)
            if !fill then
              Graphics.fill_arc sx sy srx sry a.a1 a.a2
            else
              Graphics.draw_arc sx sy srx sry a.a1 a.a2
			| FillText t -> Graphics.moveto (shapedX t.textx) (shapedY t.textx t.texty); Graphics.draw_string t.text
			| StrokeText t -> Graphics.moveto (shapedX t.textx) (shapedY t.textx t.texty); Graphics.draw_string t.text
      | ImageAt imgHdl -> (let rx = imgHdl.imgx + imgHdl.img#w and ty = imgHdl.imgy + imgHdl.img#h
          in
          let slx = shapedX imgHdl.imgx and sly = shapedY imgHdl.imgx imgHdl.imgy and
          srx = shapedX rx and sry = shapedY rx imgHdl.imgy in
          let slh = (shapedY imgHdl.imgx ty) - sly and srh = (shapedY rx ty) - sry
          in
          imgHdl.img#drawShaped slx sly srx sry slh srh)
      | SetFillStyle fs -> self#processFillStyle fs; fillStyle := fs; fill := isFillOn(fs)
      | SetLineCap       a -> ()
      | SetLineJoin      a -> ()
      | SetLineWidth     a -> self#processLineWidth a
      | SetMiterLimit    a -> ()
      | SetShadowBlur    a -> ()
      | SetShadowColor   a -> ()
      | SetShadowOffsetX a -> ()
      | SetShadowOffsetY a -> ()
      | SetStrokeStyle   a -> () 
      | SetGlobalAlpha   a -> ()
      | SetFont          a -> self#processFont a
      | SetTextAlign     a -> ()
      | SetTextBaseline  a -> ()
      | SetProperties    a -> self#processStyle a
(*      | Close -> ()*)
    in
      Array.iter treatStep motif#getPath;
            
      self#processStyle mProperties;


  method drawMotifShaped2 motif trX trY tlX tlY brX brY blX blY =

		let xo = mArea#x and yo = mArea#y in
    let tW = trX - tlX and bW = brX - blX and rH = trY - brY and lH = tlY - blY
		in
		let dW = tW - bW and dH = rH - lH and dlX = tlX - blX and dbY = brY - blY in
		let wi = motif#w and hi = motif#h
		in
    let shapedX x y = (x * (bW + (dW * y) / hi)) / wi + blX + (dlX * y) / hi + xo
    and shapedY x y = (y * (lH + (dH * x) / wi)) / hi + blY + (dbY * x) / wi + yo
    in
    let shapedXY x y = (shapedX x y, shapedY x y) in let ptConv (x, y) = shapedXY x y
    and fillStyle = ref mProperties.fillStyle and fill = ref (isFillOn mProperties.fillStyle)
    in
    let treatStep step = match step with
      | MoveTo p -> Graphics.moveto (shapedX p.x p.y) (shapedY p.x p.y)
      | Points pts -> Graphics.plots (Array.map ptConv pts)
      | Lines pts -> Graphics.draw_poly_line (Array.map ptConv pts)
      | FillRect (x, y, w, h) -> ( let rx = x + w and ty = y + h in
				match !fillStyle with
				| Gradient g -> fillGradientPolygon [|(shapedXY x y);(shapedXY rx y);(shapedXY rx ty);(shapedXY x ty)|] g
		  	| _ -> Graphics.fill_poly[|(shapedXY x y); (shapedXY rx y); (shapedXY rx ty); (shapedXY x ty)|] )
			| StrokeRect (x, y, w, h) -> let rx = x + w and ty = y + h in
				Graphics.draw_poly[|(shapedXY x y); (shapedXY rx y); (shapedXY rx ty); (shapedXY x ty)|]
      | Polygon pts -> if !fill then
                         Graphics.fill_poly (Array.map ptConv pts)
                       else
                         Graphics.draw_poly (Array.map ptConv pts)
      | BezierCurve bc -> Graphics.moveto (shapedX bc.bcsx bc.bcsy) (shapedY bc.bcsx bc.bcsy);
          Graphics.curveto (shapedXY bc.bccp1x bc.bccp1y) (shapedXY bc.bccp2x bc.bccp2y) (shapedXY bc.bcex bc.bcey)
      | Arc a ->
          let sx = shapedX a.cx a.cy and sy = shapedY a.cx a.cy in
					let srx = (shapedX (a.cx + a.rx) a.cy) - sx
					and sry = (shapedY a.cx (a.cy + a.ry)) - sy
					in
	        if !fill then Graphics.fill_arc sx sy srx sry a.a1 a.a2
	        else Graphics.draw_arc sx sy srx sry a.a1 a.a2
			| FillText t -> Graphics.moveto (shapedX t.textx t.texty) (shapedY t.textx t.texty); Graphics.draw_string t.text
			| StrokeText t -> Graphics.moveto (shapedX t.textx t.texty) (shapedY t.textx t.texty); Graphics.draw_string t.text
      | ImageAt imgHdl -> (let rx = imgHdl.imgx + imgHdl.img#w and ty = imgHdl.imgy + imgHdl.img#h
          in
          let slx = shapedX imgHdl.imgx imgHdl.imgy and sly = shapedY imgHdl.imgx imgHdl.imgy and
          srx = shapedX rx imgHdl.imgy and sry = shapedY rx imgHdl.imgy in
          let slh = (shapedY imgHdl.imgx ty) - sly and srh = (shapedY rx ty) - sry
          in
          imgHdl.img#drawShaped slx sly srx sry slh srh)
      | SetFillStyle fs -> self#processFillStyle fs; fillStyle := fs; fill := isFillOn(fs)
      | SetLineCap       a -> ()
      | SetLineJoin      a -> ()
      | SetLineWidth     a -> self#processLineWidth a
      | SetMiterLimit    a -> ()
      | SetShadowBlur    a -> ()
      | SetShadowColor   a -> ()
      | SetShadowOffsetX a -> ()
      | SetShadowOffsetY a -> ()
      | SetStrokeStyle   a -> () 
      | SetGlobalAlpha   a -> ()
      | SetFont          a -> self#processFont a
      | SetTextAlign     a -> ()
      | SetTextBaseline  a -> ()
      | SetProperties    a -> self#processStyle a
(*      | Close -> ()*)
    in
      Array.iter treatStep motif#getPath;
            
      self#processStyle mProperties;

  method clearRect x y w h =
		let ax = x + mArea#x and ay = y + mArea#y
		in
		match mBackground with
      | Image img -> img#draw ax ay;
      | Color c -> set_color c; Graphics.draw_rect ax ay w h;
      | PlainRgb c -> set_color c; Graphics.fill_rect ax ay w h;
      | PlainRgba v -> set_color (rgb v.red v.green v.blue); Graphics.fill_rect ax ay w h;
      | PlainRgbaString s -> set_color (getColorOfString s); Graphics.fill_rect ax ay w h;
      | Gradient g -> fillGradientPolygon [|(ax, ay); ((ax + w), ay); ((ax + w), (ay + h)); (ax, (ay + h))|] g;
    
    self#processFillStyle mProperties.fillStyle

	method clear() =
		let xo = mArea#x and yo = mArea#y and wo = mArea#w and ho = mArea#h in
		match mBackground with
      | Image img -> img#draw xo yo;
      | Color c -> set_color c; Graphics.draw_rect xo yo wo ho;
      | PlainRgb c -> set_color c; Graphics.fill_rect xo yo wo ho;
      | PlainRgba v -> set_color (rgb v.red v.green v.blue); Graphics.fill_rect xo yo wo ho;
      | PlainRgbaString s -> set_color (getColorOfString s); Graphics.fill_rect xo yo wo ho;
      | Gradient g ->
				fillGradientPolygon [|(xo, yo); ((xo + wo), yo); ((xo + wo), (yo + ho)); (xo, (yo + ho))|] g;
				mBackground <- Image(self#getImage());

    self#processFillStyle mProperties.fillStyle

(*
  method createLinearGradient (*x0 y0 x1 y1*) angle =
		if angle > 45 && angle < 135 then (new cGradient Vertical (*x0 y0 0 x1 y1 0*))
		else (new cGradient Horizontal (*x0 y0 0 x1 y1 0*))

  method createRadialGradient() (*x0 y0 r0 x1 y1 r1*) = new cGradient Radial (*x0 y0 r0 x1 y1 r1*)
*)
 
  method save() = mMatrix#multiply mMatrix#identity
(*    let o = new cGraphicCanvas in
    o#copyState self;
    mStateStack <- (o :: mStateStack);
    mMatrixStack <- mMatrix :: mMatrixStack;*)
    
    (*new G_VmlCanvas.Matrix(mMatrix.x, mMatrix.y, mMatrix.rot, mMatrix.xScale, mMatrix.yScale);*)

  method restore() =
    match mStateStack with
      | hd::l -> self#copyState hd
      | _ -> ();
    match mMatrixStack with
      | hd::l -> mMatrix <- hd
      | _ -> ()
        ;

  method copyState (o : 's) = self#setProperties (o#getProperties())

end;;

  (*  
method dec2hex = [];
(function () {
  for (var i = 0; i < 16; i++) {
    for (var j = 0; j < 16; j++) {
      G_VmlCanvas.dec2hex[i * 16 + j] = i.toString(16) + j.toString(16);
    }
  }
})();

method processStyle styleString =
  var str, alpha = 1.0;

  styleString = String(styleString);
  if (styleString.substring(0, 3) == "rgb") {
    var start = styleString.indexOf("(", 3);
    var end = styleString.indexOf(")", start + 1);
    var guts = styleString.substring(start + 1, end).split(",");

    str = "#";
    for (var i = 0; i < 3; i++) {
      str += G_VmlCanvas.dec2hex[parseInt(guts[i])];
    }
open CFmotif;;

    if ((guts.length == 4) && (styleString.substr(3, 1) == "a")) {
      alpha = guts[3];
    }
  } else {
    str = styleString;
  }

  return [str, alpha];
}

method processLineCap lineCap =
  switch (lineCap) {
    case 'butt': return 'flat';
    case 'round': return 'round';
    case 'square': return 'square';
  }
};
*)
(*  (** back-reference to the canvas *)
  readonly attribute htmlCanvasElement canvas;

  (** transformations (default transform is the identity matrix) *)
  method scale x y = ()
  method rotate angle = ()
  method translate x y = ()
  method transform m11 m12 m21 m22 dx dy = ()
  method setTransform m11 m12 m21 m22 dx dy = ()

  (** compositing *)
           attribute globalAlpha; (** (default 1.0) *)
           attribute DOMString globalCompositeOperation; (** (default source-over) *)

  (** colors and styles *)
           attribute any strokeStyle; (** (default black) *)
           attribute any fillStyle; (** (default black) *)
  CanvasGradient createLinearGradient x0 y0 x1 y1 = ()
  CanvasGradient createRadialGradient x0 y0 r0 x1 y1 r1 = ()
  CanvasPattern createPattern HTMLImageElement image DOMString repetition = ()
  CanvasPattern createPattern HTMLCanvasElement image DOMString repetition = ()
  CanvasPattern createPattern HTMLVideoElement image DOMString repetition = ()

  (** line caps/joins *)
           attribute lineWidth; (** (default 1) *)
           attribute DOMString lineCap; (** "butt", "round", "square" (default "butt") *)
           attribute DOMString lineJoin; (** "round", "bevel", "miter" (default "miter") *)
           attribute miterLimit; (** (default 10) *)

  (** shadows *)
           attribute shadowOffsetX; (** (default 0) *)
           attribute shadowOffsetY; (** (default 0) *)
           attribute shadowBlur; (** (default 0) *)
           attribute DOMString shadowColor; (** (default transparent black) *)

  (** focus management *)
  boolean drawFocusRing Element element xCaret yCaret optional boolean canDrawCustom = ()

  (** drawing images *)
  method drawImage HTMLImageElement image dx dy optional dw dh = ()
  method drawImage HTMLImageElement image sx sy sw sh dx dy dw dh = ()
  method drawImage HTMLCanvasElement image dx dy optional dw dh = ()
  method drawImage HTMLCanvasElement image sx sy sw sh dx dy dw dh = ()
  method drawImage HTMLVideoElement image dx dy optional dw dh = ()
  method drawImage HTMLVideoElement image sx sy sw sh dx dy dw dh = ()

  (** pixel manipulation *)
  ImageData createImageData sw sh = ()
  ImageData createImageData ImageData imagedata = ()
  ImageData getImageData sx sy sw sh = ()
  method putImageData ImageData imagedata dx dy optional dirtyX dirtyY dirtyWidth dirtyHeight = ()
*)
(*    
  method drawImage image ?(dx = 0) ?(dy = 0) ?(dw = 0) ?(dh = 0) ?(sx = 0) ?(sy = 0) ?(sw = 0) ?(sh = 0) =
  w = image.width;
  h = image.height;

  if (arguments.length == 3) {
    dx = arguments[1];
    dy = arguments[2];
    sx = sy = 0;
    sw = dw = w;
    sh = dh = h;
  } else if (arguments.length == 5) {
    dx = arguments[1];
    dy = arguments[2];
    dw = arguments[3];
    dh = arguments[4];
    sx = sy = 0;
    sw = w;
    sh = h;
  } else if (arguments.length == 9) {
    sx = arguments[1];
    sy = arguments[2];
    sw = arguments[3];
    sh = arguments[4];
    dx = arguments[5];
    dy = arguments[6];
    dw = arguments[7];
    dh = arguments[8];
  } else {
    throw "Invalid number of arguments";
  }

  d = self#Coords(dx, dy = ()

  w2 = (sw / 2 = ()
  h2 = (sh / 2 = ()

  vmlStr = [];

  (* For some reason that I've now forgotten, using divs didn't work *)
  vmlStr.push(' <g_vml_:group',
              ' coordsize="100,100"',
              ' coordorigin="0, 0"' ,
              ' style="width:100px; height:100px; position:absolute; ' = ()

(* If filters are necessary (rotation exists), create them
   filters are bog-slow, so only create them if abbsolutely necessary
   The following check doesn't account for skews (which don't exist
   in the canvas spec (yet) anyway.
*)
  if (mMatrix[0][0] != 1 || mMatrix[0][1]) {
    filter = [];

    (* Note the 12/21 reversal *)
    filter.push("M11='",mMatrix[0][0],"',",
                "M12='",mMatrix[1][0],"',",
                "M21='",mMatrix[0][1],"',",
                "M22='",mMatrix[1][1],"',",
                "Dx='",d.x,"',",
                "Dy='",d.y,"'" = ()

    (* Bounding box calculation (need to minimize displayed area so that filters
     don't waste time on unused pixels. *)
    max = d;
    c2 = self#Coords(dx+dw, dy = ()
    c3 = self#Coords(dx, dy+dh = ()
    c4 = self#Coords(dx+dw, dy+dh = ()

    max.x = Math.max(max.x, c2.x, c3.x, c4.x = ()
    max.y = Math.max(max.y, c2.y, c3.y, c4.y = ()

    vmlStr.push(' padding:0px ', Math.floor(max.x), 'px ', Math.floor(max.y),
                'px 0px;filter:progid:DXImageTransform.Microsoft.Matrix(',
                filter.join(""), ', sizingmethod=\'clip\' = ()')
  } else {
    vmlStr.push(' top:',d.y,'px; left:',d.x,'px;')
  }

  vmlStr.push(' ">' ,
              '<g_vml_:image src="', image.src, '"',
              ' style="width:', dw, ';',
              ' height:', dh, ';"',
              ' cropleft="', sx / w, '"',
              ' croptop="', sy / h, '"',
              ' cropright="', (w - sx - sw) / w, '"',
              ' cropbottom="', (h - sy - sh) / h, '"',
              ' />',
              ' </g_vml_:group>' = ()

(*  self#element.innerHTML += vmlStr.join("" = () *)
  self#element.insertAdjacentHTML("BeforeEnd",
                                  vmlStr.join("") = ()
*)
  (*
  method moveTo x y = Queue.push (MoveTo({x = x; y = y})) mPath

  method lineTo x y = Queue.push LineTo({x = x; y = y}) mPath

  method polygonTo x y = Queue.push PolygonTo({x = x; y = y}) mPath
    
  method bezierCurveTo cp1x cp1y cp2x cp2y x y =
(*    Queue.push BezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y) mPath *)
    Queue.push BezierCurveTo({cp1x = cp1x; cp1y = cp1y; cp2x = cp2x; cp2y = cp2y; ex = x; ey = y}) mPath
    
  method quadraticCurveTo cpx cpy x y = bezierCurveTo cpx cpy cpx cpy x y

  method arc x y radius startAngle endAngle aClockwise =
    match aClockwise with
    | true -> Queue.push Arc({cx = x; cy = y; rx = radius; ry = radius; a1 = startAngle; a2 = endAngle}) mPath
    | false -> Queue.push Arc({cx = x; cy = y; rx = radius; ry = radius; a1 = endAngle; a2 = startAngle}) mPath

  method closePath = Queue.push Close mPath

  method clearRect = (*    Queue.clear mPath; *) Graphics.clear_graph

  method beginPath = Queue.clear mPath
 *  * *)
        (*
  method stroke aFill =
    let strokeColor, fillColor, opacity;
  lineStr = [];
  lineOpen = false;

  if (aFill) {
    a = G_VmlCanvas.ProcessStyle(self#fillStyle = ()
    fillColor = a[0];
    opacity = a[1] * self#globalAlpha;
  } else {
    a = G_VmlCanvas.ProcessStyle(self#strokeStyle = ()
    strokeColor = a[0];
    opacity = a[1] * self#globalAlpha;
  }

  lineStr.push('<g_vml_:shape',
               ' fillcolor="', fillColor, '"',
               ' filled="',(aFill) ? "true" "false",'"',
               ' style="position:absolute; width:10; height:10;"',
               ' coordorigin="0 0" coordsize="10 10"',
               ' stroked="',(aFill) ? "false" "true",'"',
               ' strokeweight="', self#lineWidth, '"',
               ' strokecolor="', strokeColor, '"',
               ' mPath="' = ()

  newSeq = false;
  min = {x:null, y:null};
  max = {x:null, y:null};

  for i = 0 to mPath.length do
    p = mPath[i];

    if (p.type == 'moveTo') {
      lineStr.push(' m ' = ()
      c = self#Coords(p.x, p.y = ()
      lineStr.push(Math.floor(c.x), ',', Math.floor(c.y) = ()
    } else if (p.type == 'lineTo') {
      lineStr.push(' l ' = ()
      c = self#Coords(p.x, p.y = ()
      lineStr.push(Math.floor(c.x), ',', Math.floor(c.y) = ()
    } else if (p.type == 'close') {
      lineStr.push(' x ' = ()
    } else if (p.type == 'bezierCurveTo') {
      lineStr.push(' c ' = ()
      c = self#Coords(p.x, p.y = ()
      c1 = self#Coords(p.cp1x, p.cp1y = ()
      c2 = self#Coords(p.cp2x, p.cp2y = ()
      lineStr.push(Math.floor(c1.x), ',', Math.floor(c1.y), ',',
                   Math.floor(c2.x), ',', Math.floor(c2.y), ',',
                   Math.floor(c.x), ',', Math.floor(c.y) = ()
    } else if (p.type == 'arc') {
      lineStr.push(' ar ' = ()
      c  = self#Coords(p.x, p.y = ()
      cStart = self#Coords(p.xStart, p.yStart = ()
      cEnd = self#Coords(p.xEnd, p.yEnd = ()

      (* TODO(glen): FIX (matricies (scale+rotation) now buggered self up)
      //             VML arc also doesn't seem able to do rotated non-circular
      //             arcs without parent grouping. *)
      absXScale = mMatrix[0][0];
      absYScale = mMatrix[1][1];

      lineStr.push(Math.floor(c.x - absXScale * p.radius), ',', Math.floor(c.y - absYScale * p.radius), ' ',
                   Math.floor(c.x + absXScale * p.radius), ',', Math.floor(c.y + absYScale * p.radius), ' ',
                   Math.floor(cStart.x), ',', Math.floor(cStart.y), ' ',
                   Math.floor(cEnd.x), ',', Math.floor(cEnd.y) = ()
    }

    (* TODO(glen): Following is broken for curves due to
    //             move to proper paths.
    // Figure out dimensions so we can do gradient fills
    // properly *)
    if (min.x == null || c.x < min.x) {
      min.x = c.x;
    }
    if (max.x == null || c.x > max.x) {
      max.x = c.x;
    }
    if (min.y == null || c.y < min.y) {
      min.y = c.y;
    }
    if (max.y == null || c.y > max.y) {
      max.y = c.y;
    }
  }
  lineStr.push(' ">' = ()

  if (typeof self#fillStyle == 'object') {
    focus = {x:"50%", y:"50%"};
    width = (max.x - min.x = ()
    height = (max.y - min.y = ()
    dimension = (width > height) ? width height;

    focus.x = Math.floor((self#fillStyle.focus.x / width) * 100 + 50) + '%';
    focus.y = Math.floor((self#fillStyle.focus.y / height) * 100 + 50) + '%';

    colors = [];

    (* inside radius (%) *)
    if (self#fillStyle.type == 'gradientradial') {
      inside = (self#fillStyle.radius1 / dimension * 100 = ()

      (* percentage that outside radius exceeds inside radius *)
      expansion = (self#fillStyle.radius2 / dimension * 100) - inside;
    } else {
      inside = 0;
      expansion = 100;
    }

    insidecolor = {offset:null, color:null};
    outsidecolor = {offset:null, color:null};

    (* We need to sort 'colors' by percentage, from 0 > 100 otherwise ie won't interpret it correctly *)
    self#fillStyle.colors.sort(function (cs1, cs2) {
      return cs1.offset - cs2.offset;
    } = ()

    for (i = 0; i < self#fillStyle.colors.length; i++) {
      fs = self#fillStyle.colors[i];

      colors.push( (fs.offset * expansion) + inside,'% ',fs.color,"," = ()

      if (fs.offset > insidecolor.offset || insidecolor.offset == null) {
        insidecolor.offset = fs.offset;
        insidecolor.color = fs.color;
      }

      if (fs.offset < outsidecolor.offset || outsidecolor.offset == null) {
        outsidecolor.offset = fs.offset;
        outsidecolor.color = fs.color;
      }
    }
    colors.pop( = ()

    lineStr.push('<g_vml_:fill',
                 ' color="',outsidecolor.color,'"',
                 ' color2="',insidecolor.color,'"',
                 ' type="', self#fillStyle.type, '"',
                 ' focusposition="', focus.x, ', ', focus.y, '"',
                 ' colors="', colors.join(''), '" />' = ()
  }

  if (aFill) {
    lineStr.push('<g_vml_:fill opacity="', opacity, '" />' = ()
  } else {
    lineStr.push('<g_vml_:stroke opacity="', opacity, '" joinstyle="', self#lineJoin, '" miterlimit="', self#miterLimit, '" endcap="', G_VmlCanvas.ProcessLineCap(self#lineCap) ,'" />' = ()
  }

  lineStr.push('</g_vml_:shape>' = ()

  self#element.insertAdjacentHTML("beforeEnd", lineStr.join("") = ()

  mPath <- []
*)
