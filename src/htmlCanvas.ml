open Usual;;
open Motif;;
open Image;;
open GraphicImage;;
open Gradient;;
open Canvas;;

module Html = Dom_html

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
			mCtx##set_color (rgb c.red c.green c.blue);
			mCtx##moveto x (rOf !sby);
			mCtx##lineto x (rOf !sty);
			sby := !sby + dby;
			sty := !sty + dty;
		done
	) else (
		 trace("n(brX - blX + 1) "^soi n^", tlY "^soi tlY^", trY "^soi trY^", blX "^soi blX^", blY "^soi blY^", brX "^soi brX^", brY "^soi brY);
	)
	 
	
let fillVerticalGradientPolygon pts grad =
	let sOf r = r lsl 10 and rOf s = s asr 10 in
	let tlX = fst pts.(3) and tlY = snd pts.(3) and trX = fst pts.(2)
	and blX = fst pts.(0) and blY = snd pts.(0) and brX = fst pts.(1)
	in (*trace("tlY "^soi tlY^", trY "^soi trY^", blX "^soi blX^", blY "^soi blY^", brX "^soi brX^", brY "^soi brY);*)
	let n = tlY - blY + 1
	in
	let colors = grad#getColors n
	and slx = ref(sOf blX) and srx = ref(sOf brX) and dlx = sOf(tlX - blX) / n and drx = sOf(trX - brX) / n
	in
	for y = blY to tlY do
		let c = colors.(y - blY) in
		mCtx##set_color (rgb c.red c.green c.blue);
		mCtx##moveto (rOf !slx) y;
		mCtx##lineto (rOf !srx) y;
		slx := !slx + dlx;
		srx := !srx + drx;
	done

let fillRadialGradientPolygon pts grad = () (* TODO *)

let fillGradientPolygon pts grad =
	match grad#style with
		| Vertical -> fillVerticalGradientPolygon pts grad
		| Horizontal -> fillHorizontalGradientPolygon pts grad
		| Radial -> fillRadialGradientPolygon pts grad

class c xo yo wo ho =
object(self:'s) inherit Canvas.c xo yo wo ho

  val mutable mCnv = Html.createCanvas Html.window##document
  val mutable mCtx = mCnv##getContext (Html._2d_)

  (* Canvas context properties *)
	val mutable mBackground = PlainRgb 0x000000

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

	initializer
		mCnv##width <- wo; mCnv##height <- ho;

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

  method processLineWidth lineWidth = mCtx##set_line_width lineWidth;

  method processStyle properties =
    self#processFillStyle properties.fillStyle;
    self#processLineWidth properties.lineWidth;

  method processFont font = mCtx##set_font font;

  method getImage() =
    (new FhtmlImage.c ~img(subScreenshot mX mY mW mH) mW mH :> Image.c)

  method getSubImage x y w h =
    (new FhtmlImage.c (subScreenshot (x + mX) (y + mY) w h) w h :> Image.c)
(*    img#subScreenshot (x + mX) (y + mY) w h; 
    (img :> Image.c)*)
    
  method setImage img = self#setSubImage img 0 0 mW mH

  method setSubImage img x y w h =
      img#subScreenshot (x + mX) (y + mY) w h
  
(*  method drawImage img ?(dx = 0) ?(dy = 0) ?(dw = 0) ?(dh = 0) ?(sx = 0) ?(sy = 0) ?(sw = 0) ?(sh = 0) = *)
  method drawImage img x y = img#draw (x + mX) (y + mY)
(*
  method beginPath() = mMotif#beginPath()
  method setPathFillStyle fs = mMotif#setFillStyle fs
  method draw() = self#drawMotif mMotif 0 0
  method closePath() = mMotif#closePath()
*)
  method moveTo x y = mCtx##moveto (x + mX) (y + mY)
  method lineTo x y = mCtx##lineto (x + mX) (y + mY)
  method lines points = mCtx##draw_poly_line (Array.map (function pt -> fst pt + mX, snd pt + mY) points)

  method fillRect x y w h =
		let ax = x + mX and ay = y + mY in
		match mProperties.fillStyle with
		| Gradient g -> fillGradientPolygon [|(ax, ay); (ax + w, ay); (ax + w, ay + h); (ax, ay + h)|] g
  	| _ -> mCtx##fill_rect ax ay w h

  method strokeRect x y w h = mCtx##draw_rect (x + mX) (y + mY) w h

  method polygon points =
		match mProperties.fillStyle with
		| Gradient g -> fillGradientPolygon (Array.map (function pt -> fst pt + mX, snd pt + mY) points) g
		| Color v -> mCtx##draw_poly (Array.map (function pt -> fst pt + mX, snd pt + mY) points)
		| _ -> mCtx##fill_poly (Array.map (function pt -> fst pt + mX, snd pt + mY) points)

  method bezierCurveTo cp1x cp1y cp2x cp2y x y =
		mCtx##curveto (cp1x + mX, cp1y + mY) (cp2x + mX, cp2y + mY) (x + mX, y + mY)

  method quadraticCurveTo cpx cpy x y = self#bezierCurveTo cpx cpy cpx cpy x y

  method bezierCurve sx sy cp1x cp1y cp2x cp2y ex ey =
		mCtx##moveto (sx + mX) (sy + mY);
		self#bezierCurveTo  cp1x cp1y cp2x cp2y ex ey

  method arc x y radiusX radiusY startAngle endAngle =
		match mProperties.fillStyle with
		| Gradient g -> mCtx##fill_arc (x + mX) (y + mY) radiusX radiusY startAngle endAngle
		| Color v -> mCtx##draw_arc (x + mX) (y + mY) radiusX radiusY startAngle endAngle
  	| _ -> mCtx##fill_arc (x + mX) (y + mY) radiusX radiusY startAngle endAngle

  method fillText text x y ?maxWidth () = mCtx##moveto (x + mX) (y + mY); mCtx##draw_string text
  method strokeText text x y ?maxWidth () = mCtx##moveto (x + mX) (y + mY); mCtx##draw_string text
  method measureText text = let (w, h) = mCtx##text_size text in {tWidth = w}

  method drawMotif motif x y =
    let ax = mX + x and ay = mY + y and fillStyle = ref mProperties.fillStyle
    in
    let ptConv pt = (fst pt + ax, snd pt + ay)
    in
    let treatStep step =
			match step with
      | MoveTo p -> mCtx##moveto (p.x + ax) (p.y + ay)
      | Points pts -> mCtx##plots (Array.map ptConv pts)
      | Lines pts -> mCtx##draw_poly_line (Array.map ptConv pts)
      | FillRect (x, y, w, h) -> (
				let ax = x + mX and ay = y + mY in
				match !fillStyle with
				| Gradient g -> fillGradientPolygon [|(ax, ay); (ax + w, ay); (ax + w, ay + h); (ax, ay + h)|] g
		  	| _ -> mCtx##fill_rect ax ay w h	)
			| StrokeRect (x, y, w, h) -> mCtx##draw_rect (x + mX) (y + mY) w h
      | Polygon pts -> (
				match !fillStyle with
				| Gradient g -> fillGradientPolygon (Array.map ptConv pts) g
				| Color v -> mCtx##draw_poly (Array.map ptConv pts)
      	| _ -> mCtx##fill_poly (Array.map ptConv pts) )
			| BezierCurve bc -> mCtx##moveto (bc.bcsx + ax) (bc.bcsy + ay);
          mCtx##curveto (bc.bccp1x + ax, bc.bccp1y + ay) (bc.bccp2x + ax, bc.bccp2y + ay) (bc.bcex + ax, bc.bcey + ay)
			| Arc a -> (
				match !fillStyle with
				| Gradient g -> mCtx##fill_arc (a.cx + ax) (a.cy + ay) a.rx a.ry a.a1 a.a2
				| Color v -> mCtx##draw_arc (a.cx + ax) (a.cy + ay) a.rx a.ry a.a1 a.a2
      	| _ -> mCtx##fill_arc (a.cx + ax) (a.cy + ay) a.rx a.ry a.a1 a.a2 )
			| FillText t -> mCtx##moveto (t.textx + ax) (t.texty + ay); mCtx##draw_string t.text
			| StrokeText t -> mCtx##moveto (t.textx + ax) (t.texty + ay); mCtx##draw_string t.text
      | ImageAt imgHandler -> mCtx##draw_image (mCtx##make_image imgHandler.img#getColorMatrix) (imgHandler.imgx + ax) (imgHandler.imgy + ay)
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
    let scaledX x = mX + lX + (x * percent) / 100 and
    scaledY y = mY + bY + (y * percent) / 100 and scaled v = (v * percent) / 100 
    in
    let scaledXY x y = (scaledX x, scaledY y) and ptConv pt = (scaledX(fst pt), scaledY(snd pt))
    and fillStyle = ref mProperties.fillStyle and fill = ref (isFillOn mProperties.fillStyle)
    in
    let treatStep step = match step with
      | MoveTo p -> mCtx##moveto (scaledX p.x) (scaledY p.y)
      | Points pts -> mCtx##plots (Array.map ptConv pts)
      | Lines pts -> mCtx##draw_poly_line (Array.map ptConv pts)
      | FillRect (x, y, w, h) -> (
				let ax = scaledX x and ay = scaledY y and sw = scaled w and sh = scaled h in 
				match !fillStyle with
				| Gradient g -> fillGradientPolygon [|(ax, ay); (ax + sw, ay); (ax + sw, ay + sh); (ax, ay + sh)|] g
		  	| _ -> mCtx##fill_rect ax ay sw sh	)
			| StrokeRect (x, y, w, h) -> mCtx##draw_rect(scaledX x)(scaledY y)(scaled w)(scaled h)
      | Polygon pts -> if !fill then
                         mCtx##fill_poly (Array.map ptConv pts)
                       else
                         mCtx##draw_poly (Array.map ptConv pts)
      | BezierCurve bc -> mCtx##moveto (scaledX bc.bcsx) (scaledY bc.bcsy);
          mCtx##curveto (scaledXY bc.bccp1x bc.bccp1y) (scaledXY bc.bccp2x bc.bccp2y) (scaledXY bc.bcex bc.bcey)
      | Arc a ->
          let sx = scaledX a.cx and sy = scaledY a.cy and srx = scaled a.rx and sry = scaled a.ry
          in (*trace("cx "^soi a.cx^", cy "^soi a.cy^", rx "^soi a.rx^", ry "^soi a.ry);trace("sx "^soi sx^", sy "^soi sy^", srx "^soi srx^", sry "^soi sry);*)
            if !fill then
              mCtx##fill_arc sx sy srx sry a.a1 a.a2
            else
              mCtx##draw_arc sx sy srx sry a.a1 a.a2
			| FillText t -> mCtx##moveto (scaledX t.textx) (scaledY t.texty); mCtx##draw_string t.text
			| StrokeText t -> mCtx##moveto (scaledX t.textx) (scaledY t.texty); mCtx##draw_string t.text
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
    let w = rX - lX and wi = motif#getWidth and hi = motif#getHeight
    in
    let shapedX x = mX + lX + (x * w) / wi and
    shapedY x y = (y * (lH + ((rH - lH) * x) / wi)) / hi + lY + mY + ((rY - lY) * x) / wi
    in
    let shapedXY x y = (shapedX x, shapedY x y) in let ptConv (x, y) = shapedXY x y
    and fillStyle = ref mProperties.fillStyle and fill = ref (isFillOn mProperties.fillStyle)
    in
    let treatStep step = match step with
      | MoveTo p -> mCtx##moveto (shapedX p.x) (shapedY p.x p.y)
      | Points pts -> mCtx##plots (Array.map ptConv pts)
      | Lines pts -> mCtx##draw_poly_line (Array.map ptConv pts)
      | FillRect (x, y, w, h) -> ( let rx = x + w and ty = y + h in
				match !fillStyle with
				| Gradient g -> fillGradientPolygon [|(shapedXY x y);(shapedXY rx y);(shapedXY rx ty);(shapedXY x ty)|] g
		  	| _ -> mCtx##fill_poly[|(shapedXY x y); (shapedXY rx y); (shapedXY rx ty); (shapedXY x ty)|] )
			| StrokeRect (x, y, w, h) -> let rx = x + w and ty = y + h in
				mCtx##draw_poly[|(shapedXY x y); (shapedXY rx y); (shapedXY rx ty); (shapedXY x ty)|]
      | Polygon pts -> if !fill then
                         mCtx##fill_poly (Array.map ptConv pts)
                       else
                         mCtx##draw_poly (Array.map ptConv pts)
      | BezierCurve bc -> mCtx##moveto (shapedX bc.bcsx) (shapedY bc.bcsx bc.bcsy);
          mCtx##curveto (shapedXY bc.bccp1x bc.bccp1y) (shapedXY bc.bccp2x bc.bccp2y) (shapedXY bc.bcex bc.bcey)
      | Arc a ->
          let sx = shapedX a.cx and sy = shapedY a.cx a.cy and srx = (shapedX(a.cx + a.rx / 2) - shapedX(a.cx - a.rx / 2))
          in let sry = (shapedY a.cx (a.cy + a.ry)) - sy
             in (*trace("cx "^soi a.cx^", cy "^soi a.cy^", rx "^soi a.rx^", ry "^soi a.ry);trace("sx "^soi sx^", sy "^soi sy^", srx "^soi srx^", sry "^soi sry);*)
            if !fill then
              mCtx##fill_arc sx sy srx sry a.a1 a.a2
            else
              mCtx##draw_arc sx sy srx sry a.a1 a.a2
			| FillText t -> mCtx##moveto (shapedX t.textx) (shapedY t.textx t.texty); mCtx##draw_string t.text
			| StrokeText t -> mCtx##moveto (shapedX t.textx) (shapedY t.textx t.texty); mCtx##draw_string t.text
      | ImageAt imgHdl -> (let rx = imgHdl.imgx + imgHdl.img#getWidth and ty = imgHdl.imgy + imgHdl.img#getHeight
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

  method clearRect x y w h =
		let ax = x + mX and ay = y + mY
		in
		match mBackground with
      | Image img -> img#draw ax ay;
      | Color c -> set_color c; mCtx##draw_rect ax ay w h;
      | PlainRgb c -> set_color c; mCtx##fill_rect ax ay w h;
      | PlainRgba v -> set_color (rgb v.red v.green v.blue); mCtx##fill_rect ax ay w h;
      | PlainRgbaString s -> set_color (getColorOfString s); mCtx##fill_rect ax ay w h;
      | Gradient g -> fillGradientPolygon [|(ax, ay); ((ax + w), ay); ((ax + w), (ay + h)); (ax, (ay + h))|] g;
    
    self#processFillStyle mProperties.fillStyle

	method clear() =
		match mBackground with
      | Image img -> img#draw mX mY;
      | Color c -> set_color c; mCtx##draw_rect mX mY mW mH;
      | PlainRgb c -> set_color c; mCtx##fill_rect mX mY mW mH;
      | PlainRgba v -> set_color (rgb v.red v.green v.blue); mCtx##fill_rect mX mY mW mH;
      | PlainRgbaString s -> set_color (getColorOfString s); mCtx##fill_rect mX mY mW mH;
      | Gradient g ->
				fillGradientPolygon [|(mX, mY); ((mX + mW), mY); ((mX + mW), (mY + mH)); (mX, (mY + mH))|] g;
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

