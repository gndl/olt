open Usual
open Image
open Style
open Gradient

type tPoints = {mutable lx:int; mutable by:int; mutable rx:int; mutable ty:int; mutable points:(int * int)array}
type tBezierCurve = {bcsx:int; bcsy:int; bccp1x:int; bccp1y:int; bccp2x:int; bccp2y:int; bcex:int; bcey:int};;
type tArc = {cx:int; cy:int; rx:int; ry:int; a1:int; a2:int};;
type tImage = {img:Image.c; imgx:int; imgy:int};;
type tText = {text:string; textx:int; texty:int; maxWidth:int};;

type tStep =
  | MoveTo           of tPoint
(*  | LineTo           of tPoint
  | PolygonTo        of tPoint
  | BezierCurveTo    of tBezierCurve*)
  | Points           of (int * int)array
  | Lines            of (int * int)array
	| FillRect         of (int * int * int * int)
	| StrokeRect       of (int * int * int * int)
  | Polygon          of (int * int)array
  | BezierCurve      of tBezierCurve
  | Arc              of tArc
  | ImageAt          of tImage
	| FillText         of tText
	| StrokeText       of tText
  | SetFillStyle     of tFill
  | SetLineCap       of tLineCap
  | SetLineJoin      of tLineJoin
  | SetLineWidth     of int
  | SetMiterLimit    of float
  | SetShadowBlur    of int
  | SetShadowColor   of int
  | SetShadowOffsetX of int
  | SetShadowOffsetY of int
  | SetStrokeStyle   of string
  | SetGlobalAlpha   of float
  | SetFont          of string
  | SetTextAlign     of tTextAlign
  | SetTextBaseline  of tTextBaseline
  | SetProperties    of tProperties
(*  | Close*)
;;

let isPolygon = function
  | Polygon p -> true
  | _ -> false
;;

let isFillStyle = function
  | SetFillStyle fs -> true
  | _ -> false
;;

let isFillOn = function
  | Color c -> false
  | _ -> true
;;
(*
let getStepPoint = function
  | MoveTo p -> p
  | LineTo p -> p
  | PolygonTo p -> p
  | _ -> {x = 0; y = 0}
;;
*)

let getAB x1 y1 x2 y2 =
  let a = (y2 - y1) / (x2 - x1) in let b = y1 - a * x1 in (a, b);;

type tFigure = IsLines | IsPolygon | IsBezierCurve | IsArc | IsImage | IsNoFigure;;

(*
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
*)
class c =
object(self:'s)

  val mutable mXMin = max_int
  val mutable mXMax = min_int
  val mutable mYMin = max_int
  val mutable mYMax = min_int
  val mutable mW = 0
  val mutable mH = 0
    
  val mutable mCurrFigure = IsNoFigure
  val mutable mCurrPoints = [|(0, 0)|]
(*  val mutable mNewFigure = true *)
(*  val mutable mCurrPoints = {lx = max_int; by = max_int; rx = min_int; ty = min_int; points = [||]}
  val mPath = Queue.create *)
(*  val mutable mPath = ([||]:tStep array)*)
  val mutable mPath = [||]

  (*val mutable element = surface;*)

  (* Canvas context properties *)

  method setWidth  v = mW <- v
  method setHeight v = mH <- v
  method setSize w h = mW <- w; mH <- h

  method getWidth()  = mW
  method getHeight() = mH
  method w = mW
  method h = mH

  method checkSize x y =
    if x < mXMin then (mXMin <- x; mW <- mXMax - mXMin);
    if x > mXMax then (mXMax <- x; mW <- mXMax - mXMin);

    if y < mYMin then (mYMin <- y; mH <- mYMax - mYMin);
    if y > mYMax then (mYMax <- y; mH <- mYMax - mYMin);
(*
  method addPoint x y = 
    if x < mCurrPoints.lx then mCurrPoints.lx <- x;
    if x > mCurrPoints.rx then mCurrPoints.rx <- x;

    if y < mCurrPoints.by then mCurrPoints.by <- y;
    if y > mCurrPoints.ty then mCurrPoints.ty <- y;
 
    mCurrPoints.points <- Array.append mCurrPoints.points [|(x, y)|]
*)

  method endFigure =
    let endFig =
    match mCurrFigure with
    | IsLines -> mPath <- Array.append mPath [|Lines mCurrPoints|];
      mCurrPoints <- [|mCurrPoints.(Array.length mCurrPoints - 1)|];
    | IsPolygon -> mPath <- Array.append mPath [|Polygon mCurrPoints|];
      mCurrPoints <- [|mCurrPoints.(Array.length mCurrPoints - 1)|];
    | _ -> ();
    in
      endFig;
      mCurrFigure <- IsNoFigure;

  method setFillStyle     v = self#endFigure; mPath <- Array.append mPath [|SetFillStyle v|]
  method setLineCap       v = self#endFigure; mPath <- Array.append mPath [|SetLineCap v|]
  method setLineJoin      v = self#endFigure; mPath <- Array.append mPath [|SetLineJoin v|]
  method setLineWidth     v = self#endFigure; mPath <- Array.append mPath [|SetLineWidth v|]
  method setMiterLimit    v = self#endFigure; mPath <- Array.append mPath [|SetMiterLimit v|]
  method setShadowBlur    v = self#endFigure; mPath <- Array.append mPath [|SetShadowBlur v|]
  method setShadowColor   v = self#endFigure; mPath <- Array.append mPath [|SetShadowColor v|]
  method setShadowOffsetX v = self#endFigure; mPath <- Array.append mPath [|SetShadowOffsetX v|]
  method setShadowOffsetY v = self#endFigure; mPath <- Array.append mPath [|SetShadowOffsetY v|]
  method setStrokeStyle   v = self#endFigure; mPath <- Array.append mPath [|SetStrokeStyle v|]
  method setGlobalAlpha   v = self#endFigure; mPath <- Array.append mPath [|SetGlobalAlpha v|]
  method setFont          v = self#endFigure; mPath <- Array.append mPath [|SetFont v|]
  method setTextAlign     v = self#endFigure; mPath <- Array.append mPath [|SetTextAlign v|]
  method setTextBaseline  v = self#endFigure; mPath <- Array.append mPath [|SetTextBaseline v|]
  method setProperties    v = self#endFigure; mPath <- Array.append mPath [|SetProperties v|]

  (** path API *)
  method beginPath() = mPath <- [||]; mCurrPoints <- [|(0, 0)|]; mCurrFigure <- IsNoFigure;

  method moveTo x y =
    self#endFigure;
    self#checkSize x y;
    mCurrPoints <- [|(x, y)|];

  method points pts =
    self#endFigure;
    for i = 0 to Array.length pts - 1 do
      self#checkSize (fst pts.(i)) (snd pts.(i));
    done;
    mPath <- Array.append mPath [|Points pts|]

  method lineTo x y =
    self#checkSize x y;
    mCurrPoints <- Array.append mCurrPoints [|(x, y)|];
    mCurrFigure <- IsLines;

  method lines pts =
    self#endFigure;
    for i = 0 to Array.length pts - 1 do
      self#checkSize (fst pts.(i)) (snd pts.(i));
    done;
    mPath <- Array.append mPath [|Lines pts|]

  method fillRect (*?fillStyle*) x y w h =
		(*match fillStyle with Some v -> self#setFillStyle v | None ->*) self#endFigure;
    self#checkSize x y;
    self#checkSize (x + w) (y + h);
    mPath <- Array.append mPath [|FillRect(x, y, w, h)|]

  method strokeRect x y w h =
    self#endFigure;
    self#checkSize x y;
    self#checkSize (x + w) (y + h);
    mPath <- Array.append mPath [|StrokeRect(x, y, w, h)|]

  method polygonTo x y =
    self#checkSize x y;
    mCurrPoints <- Array.append mCurrPoints [|(x, y)|];
    mCurrFigure <- IsPolygon;

  method polygon pts =
    self#endFigure;
    for i = 0 to Array.length pts - 1 do
      self#checkSize (fst pts.(i)) (snd pts.(i));
    done;
    mPath <- Array.append mPath [|Polygon pts|]

  method bezierCurveTo cp1x cp1y cp2x cp2y x y =
    let p = mCurrPoints.(Array.length mCurrPoints - 1) in
    self#bezierCurve (fst p) (snd p) cp1x cp1y cp2x cp2y x y;
    mCurrPoints <- [|(x, y)|];

  method quadraticCurveTo cpx cpy x y =
    self#bezierCurveTo cpx cpy cpx cpy x y

  method bezierCurve sx sy cp1x cp1y cp2x cp2y ex ey =
    self#endFigure;
    self#checkSize sx sy; self#checkSize ex ey;
    self#checkSize cp1x cp1y; self#checkSize cp2x cp2y;
    mPath <- Array.append mPath [|BezierCurve{bcsx = sx; bcsy = sy; bccp1x = cp1x; bccp1y = cp1y; bccp2x = cp2x; bccp2y = cp2y; bcex = ex; bcey = ey}|]

(*
  method moveTo x y =
    self#checkSize x y;
    mPath <- Array.append mPath [|MoveTo{x = x; y = y}|]

  method lineTo x y =
    self#checkSize x y;
    mPath <- Array.append mPath [|LineTo{x = x; y = y}|];

  method polygonTo x y =
    self#checkSize x y;
    mPath <- Array.append mPath [|PolygonTo{x = x; y = y}|]

  method bezierCurveTo cp1x cp1y cp2x cp2y x y =
    self#checkSize cp1x cp1y;
    self#checkSize cp2x cp2y;
    self#checkSize x y;
    mPath <- Array.append mPath [|BezierCurveTo{cp1x = cp1x; cp1y = cp1y; cp2x = cp2x; cp2y = cp2y; ex = x; ey = y}|]
*)
(*
  method rect x y w h = () (* TODO *)
  method arcTo x1 y1 x2 y2 radius = () (* TODO *)
*)
  method arc x y radiusX radiusY startAngle endAngle (*anticlockwise:bool*) =
    self#endFigure;
    self#checkSize (x - radiusX) (y - radiusY);
    self#checkSize (x + radiusX) (y + radiusY);
    
    mPath <- Array.append mPath [|Arc{cx = x; cy = y; rx = radiusX; ry = radiusY; a1 = startAngle; a2 = endAngle}|];
(*    match aClockwise with
      | true -> mPath <- Array.append mPath [|(Arc({cx = x; cy = y; rx = radius; ry = radius; a1 = startAngle; a2 = endAngle}))|]
      | false -> mPath <- Array.append mPath [|(Arc({cx = x; cy = y; rx = radius; ry = radius; a1 = endAngle; a2 = startAngle}))|]
      | _ -> ()
*)
  method fillText text x y w h =
    self#endFigure;
    self#checkSize x y;
    self#checkSize (x + w) (y + h);
    mPath <- Array.append mPath [|FillText{text = text; textx = x; texty = y; maxWidth = w}|]

  method strokeText text x y w h =
    self#endFigure;
    self#checkSize x y;
    self#checkSize (x + w) (y + h);
    mPath <- Array.append mPath [|StrokeText{text = text; textx = x; texty = y; maxWidth = w}|]
(*
  method fill = () (* TODO *)
  method stroke = () (* TODO *)
  method clip = () (* TODO *)
  method isPointInPath x y = false (* TODO *)
*)
  method imageAt img x y =
    self#endFigure;
    self#checkSize x y;
    self#checkSize (x + img#w) (y + img#h);
    mPath <- Array.append mPath [|ImageAt{img = img; imgx = x; imgy = y}|]

  method closePath() =
    self#endFigure;
    (*mPath <- Array.append mPath [|Close|]*)

  method getPath = mPath

  method createLinearGradient (*x0 y0 x1 y1*) angle =
		if angle < 45 || angle > 135 then
			new Gradient.c Horizontal (*x0 y0 0 x1 y1 0*)
		else
			new Gradient.c Vertical (*x0 y0 0 x1 y1 0*)

  method createRadialGradient (*x0 y0 r0 x1 y1 r1*) = new Gradient.c Radial (*x0 y0 r0 x1 y1 r1*)
end;;
(*
  method getVerticalPart lx rx =
    let motif = new c and i = ref 0 and pathLen = Array.length mPath and
    curP = ref {x = 0; y = 0} in
    let treatStep step = match step with
      | MoveTo p ->
        if p.x >= lx && p.x <= rx then
          motif#moveTo p.x p.y;
        curP := p
      | Points pts ->
				let ptsIn = ref [||]
				in
        for i = 0 to Array.length pts - 1 do
          if fst pts.(i) >= lx && fst pts.(i) <= rx then
						ptsIn := Array.append !ptsIn [|pts.(i)|];
        done;
				
				if Array.length !ptsIn > 0 then
					motif#points !ptsIn; 

      | Lines pts ->
        curP := {x = fst pts.(0); y = snd pts.(0)};

        for i = 1 to Array.length pts - 1 do
          let p = {x = fst pts.(i); y = snd pts.(i)}
          in
            if !curP.x < lx then
            (
              if p.x > lx then
              (
                let ab = getAB !curP.x !curP.y p.x p.y
                in
                motif#moveTo lx (fst ab * lx + snd ab);
    
                if p.x > rx then
                  motif#lineTo rx (fst ab * rx + snd ab)
                else
                  motif#lineTo p.x p.y
              )
            )
            else
            (
              if !curP.x > rx then
              (
                if p.x < rx then
                (
                  let ab = getAB !curP.x !curP.y p.x p.y
                  in
                  motif#moveTo rx (fst ab * rx + snd ab);
      
                  if p.x < lx then
                    motif#lineTo lx (fst ab * lx + snd ab)
                  else
                    motif#lineTo p.x p.y;
                )
              )
              else
              (
                if p.x < lx then
                (
                  let ab = getAB !curP.x !curP.y p.x p.y in
                  motif#lineTo lx (fst ab * lx + snd ab)
                )
                else
                  if p.x > rx then (
                    let ab = getAB !curP.x !curP.y p.x p.y in
                    motif#lineTo rx (fst ab * rx + snd ab)
                  )
                  else
                    motif#lineTo p.x p.y;
              )
            );
            curP := p
        done
      | Polygon pts -> () (* TODO *)
      | BezierCurve bc -> () (* TODO *)
      | Arc a -> if a.cx > lx && a.cx < rx then motif#arc a.cx a.cy a.rx a.ry a.a1 a.a2 (* TODO *)
			| FillText t -> 
      | ImageAt imgHdl -> () (* TODO *)
      | SetFillStyle fs -> motif#setFillStyle fs
      | SetLineCap       a -> motif#setLineCap a
      | SetLineJoin      a -> motif#setLineJoin a
      | SetLineWidth     a -> motif#setLineWidth a
      | SetMiterLimit    a -> motif#setMiterLimit a
      | SetShadowBlur    a -> motif#setShadowBlur a
      | SetShadowColor   a -> motif#setShadowColor a
      | SetShadowOffsetX a -> motif#setShadowOffsetX a
      | SetShadowOffsetY a -> motif#setShadowOffsetY a
      | SetStrokeStyle   a -> motif#setStrokeStyle a 
      | SetGlobalAlpha   a -> motif#setGlobalAlpha a
      | SetProperties    a -> motif#setProperties a
(*      | Close -> motif#closePath()*)
    in
      while !i < pathLen do
        treatStep (mPath.(!i));
        i := !i + 1
      done;

      motif
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

  d = self#Coords(dx, dy);

  w2 = (sw / 2);
  h2 = (sh / 2);

  vmlStr = [];

  (* For some reason that I've now forgotten, using divs didn't work *)
  vmlStr.push(' <g_vml_:group',
              ' coordsize="100,100"',
              ' coordorigin="0, 0"' ,
              ' style="width:100px; height:100px; position:absolute; ');

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
                "Dy='",d.y,"'");

    (* Bounding box calculation (need to minimize displayed area so that filters
     don't waste time on unused pixels. *)
    max = d;
    c2 = self#Coords(dx+dw, dy);
    c3 = self#Coords(dx, dy+dh);
    c4 = self#Coords(dx+dw, dy+dh);

    max.x = Math.max(max.x, c2.x, c3.x, c4.x);
    max.y = Math.max(max.y, c2.y, c3.y, c4.y);

    vmlStr.push(' padding:0px ', Math.floor(max.x), 'px ', Math.floor(max.y),
                'px 0px;filter:progid:DXImageTransform.Microsoft.Matrix(',
                filter.join(""), ', sizingmethod=\'clip\');')
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
              ' </g_vml_:group>');

(*  self#element.innerHTML += vmlStr.join(""); *)
  self#element.insertAdjacentHTML("BeforeEnd",
                                  vmlStr.join(""));
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
    a = G_VmlCanvas.ProcessStyle(self#fillStyle);
    fillColor = a[0];
    opacity = a[1] * self#globalAlpha;
  } else {
    a = G_VmlCanvas.ProcessStyle(self#strokeStyle);
    strokeColor = a[0];
    opacity = a[1] * self#globalAlpha;
  }

  lineStr.push('<g_vml_:shape',
               ' fillcolor="', fillColor, '"',
               ' filled="',(aFill) ? "true" : "false",'"',
               ' style="position:absolute; width:10; height:10;"',
               ' coordorigin="0 0" coordsize="10 10"',
               ' stroked="',(aFill) ? "false" : "true",'"',
               ' strokeweight="', self#lineWidth, '"',
               ' strokecolor="', strokeColor, '"',
               ' mPath="');

  newSeq = false;
  min = {x:null, y:null};
  max = {x:null, y:null};

  for i = 0 to mPath.length do
    p = mPath[i];

    if (p.type == 'moveTo') {
      lineStr.push(' m ');
      c = self#Coords(p.x, p.y);
      lineStr.push(Math.floor(c.x), ',', Math.floor(c.y));
    } else if (p.type == 'lineTo') {
      lineStr.push(' l ');
      c = self#Coords(p.x, p.y);
      lineStr.push(Math.floor(c.x), ',', Math.floor(c.y));
    } else if (p.type == 'close') {
      lineStr.push(' x ');
    } else if (p.type == 'bezierCurveTo') {
      lineStr.push(' c ');
      c = self#Coords(p.x, p.y);
      c1 = self#Coords(p.cp1x, p.cp1y);
      c2 = self#Coords(p.cp2x, p.cp2y);
      lineStr.push(Math.floor(c1.x), ',', Math.floor(c1.y), ',',
                   Math.floor(c2.x), ',', Math.floor(c2.y), ',',
                   Math.floor(c.x), ',', Math.floor(c.y));
    } else if (p.type == 'arc') {
      lineStr.push(' ar ');
      c  = self#Coords(p.x, p.y);
      cStart = self#Coords(p.xStart, p.yStart);
      cEnd = self#Coords(p.xEnd, p.yEnd);

      (* TODO(glen): FIX (matricies (scale+rotation) now buggered self up)
      //             VML arc also doesn't seem able to do rotated non-circular
      //             arcs without parent grouping. *)
      absXScale = mMatrix[0][0];
      absYScale = mMatrix[1][1];

      lineStr.push(Math.floor(c.x - absXScale * p.radius), ',', Math.floor(c.y - absYScale * p.radius), ' ',
                   Math.floor(c.x + absXScale * p.radius), ',', Math.floor(c.y + absYScale * p.radius), ' ',
                   Math.floor(cStart.x), ',', Math.floor(cStart.y), ' ',
                   Math.floor(cEnd.x), ',', Math.floor(cEnd.y));
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
  lineStr.push(' ">');

  if (typeof self#fillStyle == 'object') {
    focus = {x:"50%", y:"50%"};
    width = (max.x - min.x);
    height = (max.y - min.y);
    dimension = (width > height) ? width : height;

    focus.x = Math.floor((self#fillStyle.focus.x / width) * 100 + 50) + '%';
    focus.y = Math.floor((self#fillStyle.focus.y / height) * 100 + 50) + '%';

    colors = [];

    (* inside radius (%) *)
    if (self#fillStyle.type == 'gradientradial') {
      inside = (self#fillStyle.radius1 / dimension * 100);

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
    });

    for (i = 0; i < self#fillStyle.colors.length; i++) {
      fs = self#fillStyle.colors[i];

      colors.push( (fs.offset * expansion) + inside,'% ',fs.color,",");

      if (fs.offset > insidecolor.offset || insidecolor.offset == null) {
        insidecolor.offset = fs.offset;
        insidecolor.color = fs.color;
      }

      if (fs.offset < outsidecolor.offset || outsidecolor.offset == null) {
        outsidecolor.offset = fs.offset;
        outsidecolor.color = fs.color;
      }
    }
    colors.pop();

    lineStr.push('<g_vml_:fill',
                 ' color="',outsidecolor.color,'"',
                 ' color2="',insidecolor.color,'"',
                 ' type="', self#fillStyle.type, '"',
                 ' focusposition="', focus.x, ', ', focus.y, '"',
                 ' colors="', colors.join(''), '" />');
  }

  if (aFill) {
    lineStr.push('<g_vml_:fill opacity="', opacity, '" />');
  } else {
    lineStr.push('<g_vml_:stroke opacity="', opacity, '" joinstyle="', self#lineJoin, '" miterlimit="', self#miterLimit, '" endcap="', G_VmlCanvas.ProcessLineCap(self#lineCap) ,'" />');
  }

  lineStr.push('</g_vml_:shape>');

  self#element.insertAdjacentHTML("beforeEnd", lineStr.join(""));

  mPath <- []
*)
    
    