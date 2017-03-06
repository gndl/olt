
type depth = Back | Middle | Front

let sensitiveAreas = ref []
let sensitives() = !sensitiveAreas


let backShowAreas = ref []
let backShows() = !backShowAreas

let middleShowAreas = ref []
let middleShows() = !middleShowAreas

let frontShowAreas = ref []
let frontShows() = !frontShowAreas


let resetActives() = backShowAreas := []; middleShowAreas := []; frontShowAreas := []; sensitiveAreas := []

(*
let showAreas = ref []
let shows() = !showAreas
let resetActives() = showAreas := []; sensitiveAreas := []
let getShows = function Back -> !backShowAreas | Middle -> !middleShowAreas | Front -> !frontShowAreas
let getShowsRef = function Back -> backShowAreas | Middle -> middleShowAreas | Front -> frontShowAreas
let (%) l f = List.iter f l
let (+=) l e = l := e :: !l
let (-=) l e = l := List.filter(fun a -> a != e) !l
let (+<) l e = l <- e :: l
let (-<) l e = l <- List.filter(fun a -> a != e) l
*)

class c xo yo wo ho =
object (self:'a)

  val mutable mX : int = xo
  val mutable mY : int = yo
  val mutable mW : int = wo
  val mutable mH : int = ho

	val mutable mParent : c option = None
	val mutable mVisible = false
	val mutable mDepth = Back

	val mPositionChangedActions = new EventActions.c
	val mSizeChangedActions = new EventActions.c

	val mutable mOnKeyPressed : (char -> bool) option = None
	val mutable mOnClick : (unit -> unit) option = None
	val mutable mOnButtonPressed : (int -> int -> unit) option = None
	val mutable mOnButtonReleased : (int -> int -> unit) option = None
	val mutable mOnButtonPressedMouseMotion : (int -> int -> unit) option = None
	val mutable mOnButtonReleasedMouseMotion : (int -> int -> unit) option = None

  method setX      v = mX <- v; mPositionChangedActions#apply v mY
  method setY      v = mY <- v; mPositionChangedActions#apply mX v
  method setPosition x y = mX <- x; mY <- y; mPositionChangedActions#apply x y
  method setWidth  v = mW <- v; mSizeChangedActions#apply v mH
  method setHeight v = mH <- v; mSizeChangedActions#apply mW v
  method setSize w h = mW <- w; mH <- h; mSizeChangedActions#apply w h
  method setPositionAndSize x y w h = self#setPosition x y; self#setSize w h;

  method getX()      = mX
  method getY()      = mY
  method getWidth()  = mW
  method getHeight() = mH
	
  method x      = mX
  method y      = mY
  method w      = mW
  method h      = mH

	method getParent = mParent
	method setParent p = mParent <- Some p
	method resetParent = mParent <- None
	
	method isUnder x y = x >= mX && x < mX + mW && y >= mY && y < mY + mH
	method getUnder x y = if self#isUnder x y then Some (self:>c) else None
	method getArea = (self:>c)
	
	method getDepth = mDepth
	method setDepth d = mDepth <- d
	
  method appear() =
		if not mVisible then (
			mVisible <- true;
			sensitiveAreas := (self :> c) :: !sensitiveAreas
		)
  method disappear() =
		if mVisible then (
			mVisible <- false;
			sensitiveAreas := List.filter(fun a -> a != (self :> c)) !sensitiveAreas
		)

  method isVisible = mVisible

  method show() =
		if not mVisible then (
			self#appear();
			match mDepth with
			| Back -> backShowAreas := (self :> c) :: !backShowAreas
			| Middle -> middleShowAreas := (self :> c) :: !middleShowAreas
			| Front -> frontShowAreas := (self :> c) :: !frontShowAreas
		)
  method hide() =
		if mVisible then (
			self#disappear();
			match mDepth with
			| Back -> backShowAreas := List.filter(fun a -> a != (self :> c)) !backShowAreas
			| Middle -> middleShowAreas := List.filter(fun a -> a != (self :> c)) !middleShowAreas
			| Front -> frontShowAreas := List.filter(fun a -> a != (self :> c)) !frontShowAreas
		)

	method getSensitiveAreas() = !sensitiveAreas

	method getBackShowAreas() = !backShowAreas
	method getMiddleShowAreas() = !middleShowAreas
	method getFrontShowAreas() = !frontShowAreas

  method draw() = ()
  method refresh() = self#draw()

	method positionChangedActions = mPositionChangedActions
	method sizeChangedActions = mSizeChangedActions

	method onKeyPressed = mOnKeyPressed
	method onClick = mOnClick
  method onButtonPressed = mOnButtonPressed
  method onButtonReleased = mOnButtonReleased
  method onButtonPressedMouseMotion = mOnButtonPressedMouseMotion
  method onButtonReleasedMouseMotion = mOnButtonReleasedMouseMotion

  method setOnKeyPressed f = mOnKeyPressed <- Some f
  method setOnClick f = self#setOnButtonReleased(fun x y -> f()); mOnClick <- Some f
  method setOnButtonPressed f = mOnButtonPressed <- Some f
  method setOnButtonReleased f = mOnButtonReleased <- Some f;
	
  method setOnButtonPressedMouseMotion f = mOnButtonPressedMouseMotion <- Some f
  method setOnButtonReleasedMouseMotion f = mOnButtonReleasedMouseMotion <- Some f
  method setOnMouseMotion f = self#setOnButtonPressedMouseMotion f; self#setOnButtonReleasedMouseMotion f

  method resetOnKeyPressed = mOnKeyPressed <- None
  method resetOnClick = mOnClick <- None
  method resetOnButtonPressed = mOnButtonPressed <- None
  method resetOnButtonReleased = mOnButtonReleased <- None
	
  method resetOnMouseMotion = mOnButtonPressedMouseMotion <- None; mOnButtonReleasedMouseMotion <- None
  method resetOnButtonPressedMouseMotion = mOnButtonPressedMouseMotion <- None
  method resetOnButtonReleasedMouseMotion = mOnButtonReleasedMouseMotion <- None
end
