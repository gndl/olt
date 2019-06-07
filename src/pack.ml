open Usual
open Area

let xFrameRef = ref 4
let yFrameRef = ref 4

let xFrame() = !xFrameRef
let yFrame() = !yFrameRef

let setFrameSize xfs yfs = xFrameRef := xfs; yFrameRef := yfs


let verticallyPosition y a = a#setY y; y + a#h
let verticallyCentre (y, h) a = a#setY(y + (h - a#h) / 2); (y, h)

let horizontallyPosition x a = a#setX x; x + a#w
let horizontallyCentre (x, w) a = a#setX(x + (w - a#w) / 2); (x, w)

(*type orientation = Horizontal | Vertical*)
type tLayout = Top | CentreHorizontal | Bottom | Right | CentreVertical | Left

class c ?(layout = CentreHorizontal) ?(areas = []) ?(xo=0) ?(yo=0) ()=
  object (self) inherit Area.c xo yo 0 0 as super

    val mutable mLayout = layout
    val mutable mAreas : Area.c list = areas

    initializer if List.length mAreas > 0 then self#organize layout;

    method setX v =
      let dx = v - mX
      in
      if dx <> 0 then (
	List.iter(fun a  -> a#setX(a#x + dx)) mAreas;
	super#setX v)

    method setY v =
      let dy = v - mY
      in
      if dy <> 0 then (
	List.iter(fun a  -> a#setY(a#y + dy)) mAreas;
	super#setY v)

    method setPosition x y = self#setX x; self#setY y
    method setPositionAndSize x y w h = self#setX x; self#setY y; self#setSize w h

    method getFrameSize = (!xFrameRef, !yFrameRef)

    method getAreas = mAreas
    method setAreas areas =
      List.iter(fun a -> a#setParent(self :> Area.c)) areas;
      mAreas <- areas;
      self#organize mLayout

    method addArea area =
      area#setParent(self :> Area.c);
      mAreas <- mAreas @ [area];
      self#organize mLayout

    method getUnder x y =
      if self#isUnder x y then (
	let rec gu = function [] -> None
			    | a::t -> match a#getUnder x y with None -> gu t | sa -> sa
	in gu mAreas )
      else None

    method expandAreas = false

    method orient layout =
      let (xf, yf) = self#getFrameSize
      in
      let (w, h) = match layout with
      | Top | CentreHorizontal | Bottom -> let xo = mX + xf in
	  ((List.fold_left(fun x a -> a#setX x; x + a#w) xo mAreas) - mX + xf,
	   (List.fold_left(fun h1 a -> maxi h1 (a#h)) 0 mAreas) + 2 * yf)
      | Right | CentreVertical | Left -> let yo = mY + yf in
	  ((List.fold_left(fun w1 a -> maxi w1 (a#w)) 0 mAreas) + 2 * xf,
	   (List.fold_left(fun y a -> a#setY y; y + a#h) yo mAreas) - mY + yf)
      in
      self#setSize w h;

    method align layout =
      let (xf, yf) = self#getFrameSize
      in
      match layout with
      | Top -> let top = mY + mH - yf in
	  List.iter(fun a -> a#setY(top - a#h)) mAreas
      | CentreHorizontal -> let y = mY + yf and h = mH - 2 * yf in
	  if self#expandAreas then
	    List.iter(fun a -> a#setY y; a#setHeight h) mAreas
	  else
	  List.iter(fun a -> a#setY(y + (h - a#h) / 2)) mAreas
      | Bottom -> let bottom = mY + yf in
	  List.iter(fun a -> a#setY bottom) mAreas
      | Right -> let right = mX + xf in
	  List.iter(fun a -> a#setX right) mAreas
      | CentreVertical -> let x = mX + xf and w = mW - 2 * xf in
	  if self#expandAreas then
	    List.iter(fun a -> a#setX x; a#setWidth w) mAreas
	  else
	  List.iter(fun a -> a#setX(x + (w - a#w) / 2)) mAreas
      | Left -> let left = mX + mW - xf in
	  List.iter(fun a -> a#setX(left - a#w)) mAreas


    method organize layout =
      let fl = List.fold_left in
      let (xf, yf) = self#getFrameSize in
      let (aw, ah) = match layout with
      | Top | CentreHorizontal | Bottom ->
	  ((fl(fun w a -> w + a#w) 0 mAreas), (fl(fun h a -> maxi h (a#h)) 0 mAreas))
      | Right | CentreVertical | Left ->
	  ((fl(fun w a -> maxi w (a#w)) 0 mAreas), (fl(fun h a -> h + a#h) 0 mAreas))
      in
      let ax = maxi 0 (mX + xf) and ay = max 0 (mY + mH - ah - yf) in
      let aty = ay + ah
      in
      let _ = match layout with
      | Top ->
	  fl(fun x a -> a#setPosition x (aty - a#h); x + a#w) ax mAreas
      | CentreHorizontal ->
	  if self#expandAreas then
	    fl(fun x a -> a#setPosition x ay; a#setHeight ah; x + a#w) ax mAreas
	  else
	  fl(fun x a -> a#setPosition x (ay + (ah - a#h) / 2); x + a#w) ax mAreas
      | Bottom ->
	  fl(fun x a -> a#setPosition x ay; x + a#w) ax mAreas
      | Right ->
	  fl(fun ty a -> let y = ty - a#h in a#setPosition ax y; y) aty mAreas
      | CentreVertical ->
	  if self#expandAreas then
	    fl(fun ty a -> let y = ty - a#h in a#setPosition ax y; a#setWidth aw; y) aty mAreas
	  else
	  fl(fun ty a -> let y = ty - a#h in a#setPosition(ax + (aw - a#w) / 2) y; y) aty mAreas
      | Left -> let l = ax + aw in
	  fl(fun ty a -> let y = ty - a#h in a#setPosition(l - a#w) y; y) aty mAreas
      in
      mLayout <- layout;
      super#setPosition(ax - xf) (ay - yf);
      self#setSize(aw + 2 * xf) (ah + 2 * yf);

  end
