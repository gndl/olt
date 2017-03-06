type tLineJoin = Miter | Retim
type tLineCap = Butt | Ttub

type tFill =
  | PlainRgb of int
  | PlainRgba of Gradient.tRgba
  | PlainRgbaString of string
  | Gradient of Gradient.c
  | Image of Image.c
  | Color of int

(** "start", "end", "left", "right", "center" (default: "start") *)
type tTextAlign = Start | End | Left | Right | Center

(** "top", "hanging", "middle", "alphabetic", "ideographic", "bottom" (default: "alphabetic") *)
type tTextBaseline = Top | Hanging | Middle | Alphabetic | Ideographic | Bottom

type tProperties = {
  mutable fillStyle : tFill;
  mutable lineCap : tLineCap;
  mutable lineJoin : tLineJoin;
  mutable lineWidth : int;
  mutable miterLimit : float;
  mutable shadowBlur : int;
  mutable shadowColor : int;
  mutable shadowOffsetX : int;
  mutable shadowOffsetY : int;
  mutable strokeStyle : string;
  mutable globalAlpha : float;
	mutable font : string; (** (default 10px sans-serif) *)
	mutable textAlign : tTextAlign;
	mutable textBaseline : tTextBaseline;
}


let black   = PlainRgb 0x000000
let white   = PlainRgb 0xFFFFFF
let grey    = PlainRgb 0x7F7F7F
let red     = PlainRgb 0xFF0000
let green   = PlainRgb 0x00FF00
let blue    = PlainRgb 0x0000FF
let yellow  = PlainRgb 0xFFFF00
let cyan    = PlainRgb 0x00FFFF
let magenta = PlainRgb 0xFF00FF

let rBackgroundFill = ref(PlainRgb 0xf0ddb5)
let backgroundFill = !rBackgroundFill
let setBackground c = rBackgroundFill := PlainRgb c

let rForegroundFill = ref(Color 0x553d3d)
let foregroundFill = !rForegroundFill
let setForeground c = rForegroundFill := Color c

let rTextColor = ref 0x000000
let textColor = !rTextColor
let textFill = Color !rTextColor
let setTextColor c = rTextColor := c

