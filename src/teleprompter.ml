let xFrameRef = ref 2
let yFrameRef = ref 2

let xFrame() = !xFrameRef
let yFrame() = !yFrameRef

let setFrameSize xfs yfs = xFrameRef := xfs; yFrameRef := yfs

class c columnNumber lineNumber =
object (self) inherit Area.c 0 0 0 0 as super
	val mCanvas = New.canvas ()
	val mMsgs = Queue.create ()
	val mutable mColumnNumber = columnNumber
	val mutable mLineNumber = lineNumber
	val mutable mShadeOffStyle = false
	val mImage = New.image()

initializer
	mCanvas#setArea (self :> Area.c);
	self#sizeChangedActions#addAction(fun w h ->
		mImage#setSize w h;
		mColumnNumber <- w / 15 - 1;
		mLineNumber <- h / 20 - 1);

	self#setColumnLineNumber columnNumber mLineNumber;
	self#draw();

	method setColumnNumber c = self#setWidth((c + 1) * 15);
	method setLineNumber l = self#setHeight((l + 1) * 20);
	method setColumnLineNumber c l = self#setSize((c + 1) * 15) ((l + 1) * 20);

	method getCanvas = (mCanvas :> Canvas.c)

	method setShadeOffStyle on = mShadeOffStyle <- on

	method post (msg:string) =
		Queue.add msg mMsgs;

		if Queue.length mMsgs > mLineNumber then ignore(Queue.pop mMsgs);
		self#draw();

	method draw() =
		mCanvas#activeStyle();
		mCanvas#clear();

		ignore(Queue.fold(fun i msg ->

			if mShadeOffStyle then (
				let r = (Style.textColor land 0xFF0000) + ((10 * i) lsl 16) in
				let g = (Style.textColor land 0x00FF00) + ((10 * i) lsl 8) in
				let b = (Style.textColor land 0x0000FF) + ((10 * i)) in
				mCanvas#setFillStyle(Style.PlainRgb (r lor g lor b));
			)
			else
				mCanvas#setFillStyle Style.textFill;

			mCanvas#fillText msg 5 (20 * i) ();
			i - 1
		) ((Queue.length mMsgs) - 1) mMsgs);

(*		mImage <- mCanvas#getImage();*)
		mImage#screenshot mX mY;

	method refresh() = mImage#draw mX mY;

end

