
let mini (x:int) (y:int) = if x < y then x else y
let maxi (x:int) (y:int) = if x > y then x else y
let minf (x:float) (y:float) = if x < y then x else y
let maxf (x:float) (y:float) = if x > y then x else y

(*
let pi = 4.0 *. atan 1.0
let pi2 = 2. *. pi

(* Degree of Radian *)
let dor r = (r *. 180.) /. pi

(* Radian of Degree *)
let rod d = (d *. pi) /. 180.

(* Angle of float *)
let aof f = if f < 0. then f +. pi2 else if f > pi2 then f -. pi2 else f

(* standard angle *)
let stdA a = if a < (-.pi) then a +. pi2 else if a > pi then a -. pi2 else a
let stdAoc x y = stdA(atan2 y x)
let stdRA x y d = stdA((atan2 y x) -. d)

let soi i = string_of_int i
let sof f = string_of_float f
let sob b = string_of_bool b
let soc c = String.make 1 c

let foi i = float_of_int i
let iof f = int_of_float f

let coi i = char_of_int i
let ioc c = int_of_char c

*)
