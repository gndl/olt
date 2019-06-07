open List

(* Gradient / Pattern Stubs *)
type tGradienStyle = Radial | Horizontal | Vertical (*Linear*);;
type tPoint = {x:int; y:int};;
type tRgba = {red : int; green : int; blue : int(*; alpha : int*)}

class c (gs:tGradienStyle) (*(x0:int) (y0:int) (r0:int) (x1:int) (y1:int) (r1:int)*) =
  object
    method style = gs
    (*  val mutable radius1 = r0
        val mutable radius2 = r1
        val mutable focus = {x = x0; y = y0}*)
    val mutable mColorStops = ([]:(int * tRgba) list)
    val mutable mRange = 0

    method addColorStop offset color =
      let rec insert l = match l with
      | [] -> [(offset, color)]
      | h::t -> if offset <= fst h then (offset, color)::l else h::insert t
      in
      mColorStops <- insert mColorStops;
      if offset > mRange then mRange <- offset;

    method getColors n =
      let colors = Array.make n (snd(hd mColorStops))
      in
      let grad i1 c1 i2 c2 =
	let sOf r = r lsl 10 and rOf s = s lsr 10 and rng = i2 - i1
	in let rs = (sOf(c2.red - c1.red)) / rng
	and gs = (sOf(c2.green - c1.green)) / rng     
	and bs = (sOf(c2.blue - c1.blue)) / rng
	in
	let rec setColorAt i r g b =
	  if i < i2 then (
	    colors.(i) <- {red = rOf r; green = rOf g; blue = rOf b};
	    setColorAt (i + 1)(r + rs)(g + gs)(b + bs);
	  )
	in
	setColorAt i1 (sOf c1.red) (sOf c1.green) (sOf c1.blue);
      in
      let rec gradBracket e l = match l with
      | h::t ->
	  grad ((fst e * n) / mRange) (snd e) ((fst h * n) / mRange) (snd h);
	  gradBracket h t;
      | [] -> ()
      in
      gradBracket (hd mColorStops) (tl mColorStops);
      colors

  end;;
