open Motif;;
open Image;;
open Gradient;;
open Usual;;


(** opaque object *)
(*type pattern*)

type textMetrics = {tWidth:int};;

type imageData = {iWidth:int; iHeight:int; iData:string};;

class virtual c (area:Area.c) =
  object(self)

    val mutable mArea = area

    method getArea = mArea
    method setArea area = mArea <- area
    method getX() = mArea#x
    method getY() = mArea#y
    method getWidth() = mArea#w
    method getHeight() = mArea#h

    method x = mArea#x
    method y = mArea#y
    method w = mArea#w
    method h = mArea#h

    method virtual setBackground : Style.tFill -> unit
    method virtual setFillStyle : Style.tFill -> unit
    method virtual setLineCap : Style.tLineCap -> unit
    method virtual setLineJoin : Style.tLineJoin -> unit
    method virtual setLineWidth : int -> unit
    method virtual setMiterLimit : float -> unit
    method virtual setShadowBlur : int -> unit
    method virtual setShadowColor : int -> unit
    method virtual setShadowOffsetX : int -> unit
    method virtual setShadowOffsetY : int -> unit
    method virtual setStrokeStyle : string -> unit
    method virtual setGlobalAlpha : float -> unit
    method virtual setFont : string -> unit
    method virtual setTextAlign : Style.tTextAlign -> unit
    method virtual setTextBaseline : Style.tTextBaseline -> unit

    method virtual activeStyle : unit -> unit

    method virtual setProperties : Style.tProperties -> unit
    method virtual getProperties : unit -> Style.tProperties

    method virtual getImage : unit -> Image.c
    method virtual getSubImage : int -> int -> int -> int -> Image.c
    method virtual setImage : Image.c -> unit
    method virtual setSubImage : Image.c -> int -> int -> int -> int -> unit
    (*  method virtual drawImage : Image.c -> ?dx:int -> ?dy:int -> ?dw:int -> ?dh:int -> ?sx:int -> ?sy:int -> ?sw:int -> ?sh:int -> unit *)
    method virtual drawImage : Image.c -> int -> int -> unit
    method virtual clear : unit -> unit


    (** rects *)

    method virtual clearRect : int -> int -> int -> int -> unit
    method virtual fillRect : int -> int -> int -> int -> unit
    method virtual strokeRect : int -> int -> int -> int -> unit
    (*method virtual beginPath : unit -> unit
      method virtual draw : unit -> unit
      method virtual closePath : unit -> unit
      method virtual setPathFillStyle : Style.tFill -> unit
      *)
    (*  method virtual arc : int -> int -> int -> int -> int -> bool -> unit
        method virtual strokeRect : int -> int -> int -> int -> unit*)
    method virtual moveTo : int -> int -> unit
    method virtual lineTo : int -> int -> unit
    method virtual lines : (int * int)array -> unit
    (*  method virtual rectangle : int -> int -> int -> int -> unit*)
    (*  method virtual polygonTo : int -> int -> unit*)
    method virtual polygon : (int * int)array -> unit
    method virtual bezierCurveTo : int -> int -> int -> int -> int -> int -> unit
    method virtual arc : int -> int -> int -> int -> int -> int -> unit
    (*  method virtual createLinearGradient : (*int -> int -> int -> int ->*) int -> cGradient
        method virtual createRadialGradient : (*int -> int -> int -> int -> int ->*) int -> cGradient*)
    method virtual drawMotif : Motif.c -> int -> int -> unit
    method virtual drawMotifScaled : Motif.c -> int -> int -> int -> unit
    method virtual drawMotifShaped : Motif.c -> int -> int -> int -> int -> int -> int -> unit
    method virtual drawMotifShaped2 : Motif.c -> int -> int -> int -> int -> int -> int -> int -> int -> unit


    (** text *)
    method virtual fillText : string -> int -> int -> ?maxWidth:int -> unit -> unit
    method virtual strokeText : string -> int -> int -> ?maxWidth:int -> unit -> unit
    method virtual measureText : string -> textMetrics


    (** state *)

    method virtual save : unit -> unit	(** push state on state stack *)
    method virtual restore : unit -> unit (** pop state stack and restore state *)

(*
  (** transformations (default transform is the identity matrix) *)
  method virtual scale : float -> float -> unit
  method virtual rotate : float -> unit
  method virtual translate : float -> float -> unit
  method virtual transform : float -> float -> float -> float -> float -> float -> unit
  method virtual setTransform : float -> float -> float -> float -> float -> float -> unit
*)
(*
  (** compositing *)
           attribute float globalAlpha; (** (default 1.0) *)
           attribute DOMString globalCompositeOperation; (** (default source-over) *)

  (** colors and styles *)
           attribute any strokeStyle; (** (default black) *)
           attribute any fillStyle; (** (default black) *)
  CanvasGradient createLinearGradient : float -> float -> float -> float -> unit
  CanvasGradient createRadialGradient : float -> float -> float -> float -> float -> float -> unit
  CanvasPattern createPattern : HTMLImageElement image -> DOMString repetition -> unit
  CanvasPattern createPattern : HTMLCanvasElement image -> DOMString repetition -> unit
  CanvasPattern createPattern : HTMLVideoElement image -> DOMString repetition -> unit

  (** line caps/joins *)
           attribute float lineWidth; (** (default 1) *)
           attribute DOMString lineCap; (** "butt", "round", "square" (default "butt") *)
           attribute DOMString lineJoin; (** "round", "bevel", "miter" (default "miter") *)
           attribute float miterLimit; (** (default 10) *)

  (** shadows *)
           attribute float shadowOffsetX; (** (default 0) *)
           attribute float shadowOffsetY; (** (default 0) *)
           attribute float shadowBlur; (** (default 0) *)
           attribute DOMString shadowColor; (** (default transparent black) *)



  (** focus management *)
  method drawFocusRing : Element element -> float -> float -> ?canDrawCustom:bool -> bool

  (** drawing images *)
  method virtual drawImage : HTMLImageElement image -> float -> float -> optional float dw -> float dh -> unit
  method virtual drawImage : HTMLImageElement image -> float sx -> float sy -> float sw -> float sh -> float -> float -> float dw -> float dh -> unit
  method virtual drawImage : HTMLCanvasElement image -> float -> float -> optional float dw -> float dh -> unit
  method virtual drawImage : HTMLCanvasElement image -> float sx -> float sy -> float sw -> float sh -> float -> float -> float dw -> float dh -> unit
  method virtual drawImage : HTMLVideoElement image -> float -> float -> optional float dw -> float dh -> unit
  method virtual drawImage : HTMLVideoElement image -> float sx -> float sy -> float sw -> float sh -> float -> float -> float dw -> float dh -> unit

  (** pixel manipulation *)
  ImageData createImageData : float sw -> float sh -> unit
  ImageData createImageData : ImageData imagedata -> unit
  ImageData getImageData : float sx -> float sy -> float sw -> float sh -> unit
  method virtual putImageData : ImageData imagedata -> float -> float -> optional float dirtyX -> float dirtyY -> float dirtyWidth -> float dirtyHeight -> unit
*)
  end;;
