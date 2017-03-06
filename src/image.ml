
class virtual c (width0:int) (height0:int) =
object(self)
  val mutable mW = width0
  val mutable mH = height0

  method getWidth()  = mW
  method getHeight() = mH
  method w = mW
  method h = mH

	method virtual setWidth : int -> unit
  method virtual setHeight : int -> unit
  method virtual setSize : int -> int -> unit
  method virtual getColorMatrix : int array array
  method virtual setColorMatrix : int array array -> unit
  method virtual screenshot : int -> int -> unit
  method virtual subScreenshot : int -> int -> int -> int -> unit
  method virtual getShaped : int -> int -> int -> int -> int -> int -> c
  method virtual getScaled : int -> c
  method virtual getRotated : unit -> c
  method virtual draw : int -> int -> unit
  method virtual drawShaped : int -> int -> int -> int -> int -> int -> unit
  method virtual drawScaled : int -> int -> int -> unit
  method virtual drawRotated : int -> int -> unit
    
end;;
