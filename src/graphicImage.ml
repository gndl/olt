open Graphics
open Image
open Usual

let subScreenshot x y w h = Graphics.get_image x y w h

class c ?image ?colorMatrix ?(subScreenshot=false) ?(xo=0) ?(yo=0) wo ho =
  object(self) inherit Image.c wo ho
(*
  val mutable mImage : Graphics.image option = None
val mutable mImage = match image with Some i -> i | None -> Graphics.create_image wo ho
*)
    val mutable mImage : Graphics.image option = image

    initializer
      match colorMatrix with
      | Some cm -> self#setColorMatrix cm
      | None -> if subScreenshot then self#subScreenshot xo yo wo ho;


    method img =
      match mImage with Some i -> i
		      | None -> let i = Graphics.create_image mW mH in mImage <- Some i; i
(*
method setImg img = mImage <- img
*)
    method setImg img w h = mImage <- Some img; mW <- w; mH <- h

    method setWidth  v = self#setSize v mH
    method setHeight v = self#setSize mW v
    method setSize w h =
      if w > 0 && h > 0 then self#setImg(Graphics.create_image w h) w h;
      (*    let cm = Array.make_matrix h w 0 and ex = min mW w and ey = min mH h in
            for x = 0 to ex do
              for y = 0 to ey do
                cm.(y).(x) <- mColorMatrix.(y).(x)
              done
            done;
            mColorMatrix <- cm;*)


    method getColorMatrix = Graphics.dump_image (self#img)
    method setColorMatrix cm =
      if Array.length cm > 0 then
	self#setImg(Graphics.make_image cm) (Array.length cm.(0)) (Array.length cm);
      (*    mColorMatrix <- cm;*)

    method screenshot x y = Graphics.blit_image(self#img) x y

    method subScreenshot x y w h = self#setImg(Graphics.get_image x y w h) w h

    method getGraphicImageShaped lY rY w lH rH =
      let minY = mini lY rY and hi = mH * 10000 in
      let rly = lY - minY and rry = rY - minY and cw = (mW * 10000) / w and cdh = ((rH - lH) * 10000) / w and cdy = ((rY - lY) * 10000) / w
      in
      let mh = maxi (rly + lH)(rry + rH) in let mi = Graphics.dump_image (self#img) and mo = Array.make_matrix mh w transp in
      (*trace("wi "^soi(Array.length mi.(0))^", hi "^soi(Array.length mi)^", w "^soi w^", mh "^soi mh^", transp "^soi transp^", cw "^soi cw);*)
      for x = 0 to w - 1 do
        let xi = (x * cw) / 10000 and h = (lH + (cdh * x) / 10000) in
        let ch = hi / h and by = rly + (cdy * x) / 10000 in
        for y = by to by + h - 1 do
          let yi = ((y - by) * ch) / 10000 in
          (*trace("x "^soi x^", y "^soi y^", xi "^soi xi^", yi "^soi yi^", cw "^soi cw);*)
          (*if yi >= 0 && yi < mH then ( *)
          mo.(mh - 1 - y).(x) <- mi.(mH - 1 - yi).(xi)
          (* ) *)
        done
      done;
      (Graphics.make_image mo)

    method getGraphicImageScaled w h =
      let mi = Graphics.dump_image (self#img) and mo = Array.make_matrix h w transp
      in
      for x = 0 to w - 1 do
        for y = 0 to h - 1 do
          mo.(y).(x) <- mi.((y * mH) / h).((x * mW) / w)
        done
      done;
      (Graphics.make_image mo)

    method getShaped lX lY rX rY lH rH =
      let minY = mini lY rY in let w = (rX - lX) and h = maxi (lY - minY + lH)(rY - minY + rH) in
      (new c ~image:(self#getGraphicImageShaped lY rY w lH rH) w h :> Image.c)

    method getScaled percent =
      let w = ((mW * percent) / 100) and h = ((mH * percent) / 100) in
      (new c ~image:(self#getGraphicImageScaled w h) w h :> Image.c)

    method getRotated() =
      let mi = Graphics.dump_image (self#img) and mo = Array.make_matrix mW mH transp and w = mW - 1 and h = mH - 1
      in
      for i = 0 to w do
        for j = 0 to h do
          mo.(w - i).(j) <- mi.(j).(i)
        done
      done;
      ((new c ~image:(Graphics.make_image mo) mH mW) :> Image.c)

    method draw x y = Graphics.draw_image (self#img) x y

    method drawRotated x y =
      let img = self#getRotated() in
      img#draw x y

    method drawShaped lX lY rX rY lH rH =
      let img = self#getGraphicImageShaped lY rY (rX - lX) lH rH in
      Graphics.draw_image img lX lY
    (*    
    let mh = maxi (lY + lH)(rY + rH) and minY = mini lY rY and w = rX - lX in
      let rly = lY - minY and cw = mW / w and cdh = (rH - lH) / w and cdy = (rY - lY) / w
      in
      let mi = Graphics.dump_image (self#img) and mo = Array.make_matrix mh w transp in
        for x = 0 to w - 1 do
          let xi = x * cw and h = (lH + cdh * x) in let ch = mH / h and by = rly + cdy * x in
          for y = by to by + h - 1 do
            let yi = (y - by) * ch in
              mo.(y).(x) <- mi.(yi).(xi)
          done
        done;
        Graphics.draw_image (Graphics.make_image mo) lX lY*)

    method drawScaled  lX lY percent =
      let w = ((mW * percent) / 100) and h = ((mH * percent) / 100) in
      let img = self#getGraphicImageScaled w h in
      Graphics.draw_image img lX lY

(*
  method drawDistorted blX blY brX brY trX trY tlX tlY =
    let minX = mini(mini blX brX)(mini tlX trX) and minY = mini(mini blY brY)(mini tlY trY)
    and maxX = maxi(maxi blX brX)(maxi tlX trX) and maxY = maxi(maxi blY brY)(maxi tlY trY)
    and tw = trX - tlX and bw = brX - blX and bdx = blX - minX and tdx = tlX - minX
    and lh = tlY - blY and rh = trY - brY and ldy = blY - minY and rdy = brY - minY in
      let dw = tw - bw and ddx = tdx - bdx and dh = rh - lh and ddy = rdy - ldy in
      let mw = maxX - minX and mh = maxY - minY in
      let mi = Graphics.dump_image (self#img) and mo = Array.make_matrix mh mw transp in
        for y = 0 to h - 1 do
          let dx = 
          for x = 0 to w - 1 do
          done
          done;
    Graphics.draw_image (Graphics.make_image mo) minX minY
*)
  end;;
(*
let cGraphicImageFactory w h = ((new c w h) :> Image.c);;

*)
