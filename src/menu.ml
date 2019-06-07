(*open Pack*)

let xFrameRef = ref 2
let yFrameRef = ref 2

let xFrame() = !xFrameRef
let yFrame() = !yFrameRef

let setFrameSize xfs yfs = xFrameRef := xfs; yFrameRef := yfs

let backgroundMotifRef =
  let m = new Motif.c in
  m#setFillStyle Style.backgroundFill;
  m#fillRect 0 0 100 100;
  m#setFillStyle Style.foregroundFill;
  m#strokeRect 0 0 100 100;
  ref m

let backgroundMotif() = !backgroundMotifRef
let setBackgroundMotif bg = backgroundMotifRef := bg

let backgroundImageRef : Image.c option ref = ref None
let backgroundImage() = !backgroundImageRef
let setBackgroundImage img = backgroundImageRef := Some img

class c ?(entrys:entryC list = []) ?(xo = 0) ?(yo = 0) ?parentEntry ?(volatil = true) () =
  object (self: 'a) inherit Pack.c ~layout:Pack.CentreVertical ~xo ~yo () as super

    val mutable mEntrys : entryC list = entrys
    val mutable mVolatil = volatil
    val mutable mParentEntry = parentEntry(*match parentEntry with Some e -> Some e | None -> None*)
    val mutable mImage = New.image()
    val mutable mRedrawRequired = true

    initializer
      self#setDepth Area.Middle;
      self#setOnKeyPressed(fun key -> 
	  try
	    let b = List.find(fun b -> key = b#getShortcut) mEntrys
	    in b#click(); true
	  with Not_found -> false);

      self#sizeChangedActions#addAction(fun w h ->
	  mRedrawRequired <- true;
	  if self#isVisible then (
	    self#draw()
	  );
	);

      self#setEntrys entrys;

    method getFrameSize = (!xFrameRef, !yFrameRef)

    method expandAreas = true

    method getParentEntry = mParentEntry
    method setParentEntry p = mParentEntry <- Some p

    (*method getParentMenu = match mParentEntry with Some e -> e#getParentMenu | n -> n*)

    method getEntrys = mEntrys
    method setEntrys entrys =
      mEntrys <- entrys;
      List.iter(fun e -> e#setParentMenu(self :> c)) entrys;
      self#setAreas(List.map(fun e -> e#getArea) entrys)

    method addEntry e =
      mEntrys <- e :: mEntrys;
      e#setParentMenu(self :> c);
      self#addArea e#getArea

    method getVolatil = mVolatil
    method setVolatil v =
      List.iter(fun e -> e#setHideParentOnClick v) mEntrys;
      mVolatil <- v

    method build entrys = self#setEntrys entrys;

    method buildAt xo yo entrys =
      self#setX xo; self#setY yo;
      self#setEntrys entrys;

    method disappear() =
      List.iter(fun e -> match e#getSubMenu with None -> ()
		                               | Some sm -> if sm#isVisible then sm#disappear()) mEntrys;

      if mVolatil then (
	super#disappear();
	match mParentEntry with None -> () | Some e ->
	match e#getParentMenu with Some pm -> pm#disappear() | None -> ()
      )

    method draw() =
      if mRedrawRequired then (
	mRedrawRequired <- false;

	if mImage#w != mW || mImage#h != mH then
	  ignore(match backgroundImage() with
	    | Some img -> mImage <- img#getShaped 0 0 mW 0 mH mH
	    | None -> mImage#setSize mW mH);

	let cnv = New.canvas ~x:mX ~y:mY ~w:mW ~h:mH ()
	in
	(*		cnv#clear();
	  cnv#setFillStyle Style.foregroundFill;
	  cnv#strokeRect 0 0 mW mH;*)
	cnv#drawMotifShaped (backgroundMotif()) 0 0 mW 0 mH mH;

	List.iter(fun e -> e#draw()) mEntrys;
	ignore(List.exists(fun e -> e#drawSubMenu()) mEntrys);

	mImage#screenshot mX mY;
	(*		mImage <- cnv#getImage();*)
      )

    method refresh() =
      mImage#draw mX mY;
      List.iter(fun e -> e#refresh()) mEntrys;
(*
method getImage = mImage
method setImage img = mImage <- img
*)
  end



and entryC ?(label = "") ?(shortcut = Char.chr 0) ?(act = (fun e->()))
    ?(subMenu=None) ?(subMenuEntrys:entryC list = []) () =
  object (self)
    inherit Fbutton.c ~label ~shortcut ~hideParentOnClick:true () as super

    val mutable mParentMenu : c option = None
    val mutable mSubMenu : c option = subMenu

    initializer
      self#setDepth Area.Middle;

      self#setOnClick(fun() ->
	  let maskOtherSubMenus() =
	    match mParentMenu with None -> ()
				 | Some pm -> (
				     let vpm = pm#getVolatil in pm#setVolatil false;
				     List.iter(fun e -> match e#getSubMenu with None -> () | Some sm ->
					 if sm#isVisible then sm#disappear()) pm#getEntrys;
				     pm#setVolatil vpm)
	  in
	  let doAct = if mSubMenu == None then (act(self :> entryC); false) else true
	  in
	  match mSubMenu with None -> maskOtherSubMenus()
			    | Some sm -> (
				self#setHideParentOnClick false;

				if sm#isVisible then (
				  let vsm = sm#getVolatil in sm#setVolatil true;
				  sm#disappear();
				  sm#setVolatil vsm
				) else (
				  if doAct then act(self :> entryC);
				  maskOtherSubMenus();
				  sm#appear();
				  ignore(self#drawSubMenu());
				)
			      )
	);

      match mSubMenu with Some m -> m#setParentEntry(self :> entryC)
		        | None -> if subMenuEntrys <> [] then self#buildSubMenu subMenuEntrys;


    method getParentMenu = mParentMenu
    method setParentMenu p = mParentMenu <- Some p

    method getSubMenu = mSubMenu
    method setSubMenu sm = sm#setParentEntry(self :> entryC); mSubMenu <- Some sm
    method createSubMenu =
      let sm = new c~parentEntry:(self :> entryC)() in mSubMenu <- Some sm; sm

    method buildSubMenu entrys =
      let sm = match mSubMenu with None -> self#createSubMenu | Some m -> m
      in sm#build entrys;

    method drawSubMenu() =
      match mSubMenu with None -> false
		        | Some m ->
			    if m#isVisible then (
			      m#setPosition(mX + (mW / 2)) (mY + (mH / 2) - m#h);
			      m#draw();
			      true)
			    else false

    method draw() =
      match mParentMenu with Some _ -> super#drawText()
		           | None -> super#draw()


    method refresh() =
      let _ = match mParentMenu with Some _ -> ()
		                   | None -> super#draw();
      in
      match mSubMenu with None -> ()
		        | Some m -> if m#isVisible then m#refresh()


  end


let entry ?(label = "") ?(shortcut = Char.chr 0)
    ?(act = (fun e -> ())) ?subMenu ?(subMenuEntrys:entryC list = []) ()=
  new entryC ~label ~shortcut ~act ~subMenu ~subMenuEntrys ()
