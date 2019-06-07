open Style
open Motif

let xFrameRef = ref 10
let yFrameRef = ref 3

let xFrame = !xFrameRef
let yFrame = !yFrameRef

let setFrameSize xfs yfs = xFrameRef := xfs; yFrameRef := yfs


class c ?(label="") ?(shortcut=Char.chr 0) ?(fontSize=12)
    ?(act=(fun() -> ())) ?(hideParentOnClick=false) ?(dataId=0) () =
  object (self) inherit Area.c 0 0 0 0 as super 
    val mCanvas = New.canvas ()
    (*	val mIcon = ho / 20 - 1*)
    val mutable mLabel = label
    val mutable mShortcut = shortcut
    val mutable mFontSize = fontSize
    val mutable mDataId = dataId
    val mutable mHideParentOnClick = hideParentOnClick

    initializer 
      mCanvas#setArea (self :> Area.c);
      self#setLabel label;
      self#setFontSize fontSize;
      self#setOnClick act;
      self#setOnButtonPressed(fun x y -> ());

      self#setOnKeyPressed(fun key ->
	  if key = self#getShortcut then ( self#click(); true ) else false);


    method getCanvas = (mCanvas :> Canvas.c)

    method getLabel = mLabel

    method setLabel l =
      mLabel <- l;
      let (w, h) = Graphics.text_size l in

      self#setWidth(w + 2 * !xFrameRef);
      self#setHeight(h + 2 * !yFrameRef);

    method getShortcut = mShortcut
    method setShortcut v = mShortcut <- v

    method setFontSize fs =
      mFontSize <- fs;
      self#setHeight(fs + (2 * !yFrameRef));

    method getDataId = mDataId
    method setDataId di = mDataId <- di
    method setHideParentOnClick v = mHideParentOnClick <- v

    method click() = match super#onClick with Some f -> f() | None -> ()

    method setOnClick f = super#setOnClick(fun() -> f();
		                            if mHideParentOnClick then
			                      match self#getParent with Some p -> p#disappear() | None -> ())

    method drawText() =
      Graphics.set_text_size mFontSize;
      mCanvas#setFillStyle Style.textFill;
      mCanvas#fillText mLabel !xFrameRef !yFrameRef ();

    method draw() =
(*
self#setFillStyle grey;
self#setLineWidth 1;

let w = self#getWidth() and mh = self#h() / 4 in

for i = 0 to mh do
let y = i * 4 in mCanvas#lines[|(0, y); (w, y)|]; done;
*)
      mCanvas#clear();
      mCanvas#setFillStyle Style.foregroundFill;
      mCanvas#strokeRect 0 0 mW mH;
      self#drawText();

  end
