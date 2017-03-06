(* A fairly conventional 3D matrix object that can transform sets of
    3D points and perform a variety of manipulations on the transform *)

let pi = 4.0 *. atan 1.0 (*pi = 3.14159265*);;
let piPer180 = (pi /. 180.);;

type coordinate = {x:float; y:float};;

type t = {
  mutable xx:float; mutable xy:float; mutable xz:float; mutable xo:float;
  mutable yx:float; mutable yy:float; mutable yz:float; mutable yo:float;
  mutable zx:float; mutable zy:float; mutable zz:float; mutable zo:float};;

let unit = {xx = 1.; xy = 0.; xz = 0.; xo = 0.; yx = 0.; yy = 1.; yz = 0.; yo = 0.; zx = 0.; zy = 0.; zz = 1.; zo = 0.}

class c =
 object(self : 's)

  val mutable mMatrix = unit

  method get = mMatrix
  method set matrix = mMatrix <- matrix
  method copy (o :'s) = mMatrix <- o#get

  (* Reinitialize to the unit matrix *)
  method init = mMatrix <- unit


  (* Scale by f in all dimensions *)
  method scaleBy f =
    mMatrix <- {xx = mMatrix.xx *. f; xy = mMatrix.xy *. f; xz = mMatrix.xz *. f; xo = mMatrix.xo *. f;
                 yx = mMatrix.yx *. f; yy = mMatrix.yy *. f; yz = mMatrix.yz *. f; yo = mMatrix.yo *. f;
                 zx = mMatrix.zx *. f; zy = mMatrix.zy *. f; zz = mMatrix.zz *. f; zo = mMatrix.zo *. f}


  (* Scale along each axis independently *)
  method scale xf yf zf =
    mMatrix <- {xx = mMatrix.xx *. xf; xy = mMatrix.xy *. xf; xz = mMatrix.xz *. xf; xo = mMatrix.xo *. xf;
                 yx = mMatrix.yx *. yf; yy = mMatrix.yy *. yf; yz = mMatrix.yz *. yf; yo = mMatrix.yo *. yf;
                 zx = mMatrix.zx *. zf; zy = mMatrix.zy *. zf; zz = mMatrix.zz *. zf; zo = mMatrix.zo *. zf}
    
    
  (* Translate the origin *)
  method translate x y z =
    mMatrix.xo <- mMatrix.xo +. x;
    mMatrix.yo <- mMatrix.yo +. y;
    mMatrix.zo <- mMatrix.zo +. z

    
  (* rotate theta degrees about the y axis *)
  method yrot theta =
    let t = theta *. piPer180 in
    let ct = cos(t) and st = sin(t) in
	
    let nxx = (mMatrix.xx *. ct +. mMatrix.zx *. st) and
    nxy = (mMatrix.xy *. ct +. mMatrix.zy *. st) and
    nxz = (mMatrix.xz *. ct +. mMatrix.zz *. st) and
    nxo = (mMatrix.xo *. ct +. mMatrix.zo *. st) and
	
    nzx = (mMatrix.zx *. ct -. mMatrix.xx *. st) and
    nzy = (mMatrix.zy *. ct -. mMatrix.xy *. st) and
    nzz = (mMatrix.zz *. ct -. mMatrix.xz *. st) and
    nzo = (mMatrix.zo *. ct -. mMatrix.xo *. st) in
	
    mMatrix.xo <- nxo; mMatrix.xx <- nxx; mMatrix.xy <- nxy; mMatrix.xz <- nxz;
    mMatrix.zo <- nzo; mMatrix.zx <- nzx; mMatrix.zy <- nzy; mMatrix.zz <- nzz


  (* rotate theta degrees about the x axis *)
  method xrot theta =
    let t = theta *. piPer180 in
    let ct = cos(t) and st = sin(t) in
    	
    let nyx = (mMatrix.yx *. ct +. mMatrix.zx *. st) and
    nyy = (mMatrix.yy *. ct +. mMatrix.zy *. st) and
    nyz = (mMatrix.yz *. ct +. mMatrix.zz *. st) and
    nyo = (mMatrix.yo *. ct +. mMatrix.zo *. st) and
	
    nzx = (mMatrix.zx *. ct -. mMatrix.yx *. st) and
    nzy = (mMatrix.zy *. ct -. mMatrix.yy *. st) and
    nzz = (mMatrix.zz *. ct -. mMatrix.yz *. st) and
    nzo = (mMatrix.zo *. ct -. mMatrix.yo *. st) in
	
    mMatrix.yo <- nyo; mMatrix.yx <- nyx; mMatrix.yy <- nyy; mMatrix.yz <- nyz;
    mMatrix.zo <- nzo; mMatrix.zx <- nzx; mMatrix.zy <- nzy; mMatrix.zz <- nzz

 
    (* rotate theta degrees about the z axis *)
  method zrot theta =
    let t = theta *. piPer180 in
    let ct = cos(t) and st = sin(t) in
    
    let nyx = (mMatrix.yx *. ct +. mMatrix.xx *. st) and
    nyy = (mMatrix.yy *. ct +. mMatrix.xy *. st) and
    nyz = (mMatrix.yz *. ct +. mMatrix.xz *. st) and
    nyo = (mMatrix.yo *. ct +. mMatrix.xo *. st) and
	
    nxx = (mMatrix.xx *. ct -. mMatrix.yx *. st) and
    nxy = (mMatrix.xy *. ct -. mMatrix.yy *. st) and
    nxz = (mMatrix.xz *. ct -. mMatrix.yz *. st) and
    nxo = (mMatrix.xo *. ct -. mMatrix.yo *. st) in
	
    mMatrix.yo <- nyo; mMatrix.yx <- nyx; mMatrix.yy <- nyy; mMatrix.yz <- nyz;
    mMatrix.xo <- nxo; mMatrix.xx <- nxx; mMatrix.xy <- nxy; mMatrix.xz <- nxz


  (* Multiply this matrix by a second: M = M*R *)
  method multiply (rhs : 's) =
    let s = mMatrix and m = rhs#get in
    let lxx = s.xx *. m.xx +. s.yx *. m.xy +. s.zx *. m.xz and
    lxy = s.xy *. m.xx +. s.yy *. m.xy +. s.zy *. m.xz and
    lxz = s.xz *. m.xx +. s.yz *. m.xy +. s.zz *. m.xz and
    lxo = s.xo *. m.xx +. s.yo *. m.xy +. s.zo *. m.xz +. m.xo and
	
    lyx = s.xx *. m.yx +. s.yx *. m.yy +. s.zx *. m.yz and
    lyy = s.xy *. m.yx +. s.yy *. m.yy +. s.zy *. m.yz and
    lyz = s.xz *. m.yx +. s.yz *. m.yy +. s.zz *. m.yz and
    lyo = s.xo *. m.yx +. s.yo *. m.yy +. s.zo *. m.yz +. m.yo and
	
    lzx = s.xx *. m.zx +. s.yx *. m.zy +. s.zx *. m.zz and
    lzy = s.xy *. m.zx +. s.yy *. m.zy +. s.zy *. m.zz and
    lzz = s.xz *. m.zx +. s.yz *. m.zy +. s.zz *. m.zz and
    lzo = s.xo *. m.zx +. s.yo *. m.zy +. s.zo *. m.zz +. m.zo in
	
    mMatrix.xx <- lxx; mMatrix.xy <- lxy; mMatrix.xz <- lxz; mMatrix.xo <- lxo;
    mMatrix.yx <- lyx; mMatrix.yy <- lyy; mMatrix.yz <- lyz; mMatrix.yo <- lyo;
    mMatrix.zx <- lzx; mMatrix.zy <- lzy; mMatrix.zz <- lzz; mMatrix.zo <- lzo


  (* Transform nvert points from v into tv.  v contains the input
     coordinates in floating point.  Three successive entries in
     the array constitute a point.  tv ends up holding the transformed
     points as integers; three successive entries per point *)
  method transform v tv nvert =
    let lxx = mMatrix.xx and lxy = mMatrix.xy and lxz = mMatrix.xz and lxo = mMatrix.xo and
    lyx = mMatrix.yx and lyy = mMatrix.yy and lyz = mMatrix.yz and lyo = mMatrix.yo and
    lzx = mMatrix.zx and lzy = mMatrix.zy and lzz = mMatrix.zz and lzo = mMatrix.zo in

    let i = ref (nvert * 3) in
    while !i >= 0 do
      let x = v.(!i) and
      y = v.(!i + 1) and
      z = v.(!i + 2) in
      tv.(!i    ) <- int_of_float (x *. lxx +. y *. lxy +. z *. lxz +. lxo);
      tv.(!i + 1) <- int_of_float (x *. lyx +. y *. lyy +. z *. lyz +. lyo);
      tv.(!i + 2) <- int_of_float (x *. lzx +. y *. lzy +. z *. lzz +. lzo);
      i := !i - 3
    done


  method toString =
	"[" ^ string_of_float mMatrix.xo ^ "," ^ string_of_float mMatrix.xx ^ "," ^ string_of_float mMatrix.xy ^ "," ^ string_of_float mMatrix.xz ^ ";"
    ^ string_of_float mMatrix.yo ^ "," ^ string_of_float mMatrix.yx ^ "," ^ string_of_float mMatrix.yy ^ "," ^ string_of_float mMatrix.yz ^ ";"
    ^ string_of_float mMatrix.zo ^ "," ^ string_of_float mMatrix.zx ^ "," ^ string_of_float mMatrix.zy ^ "," ^ string_of_float mMatrix.zz ^ "]"

end;;


type tSimpleMatrix3D = {
  mutable xx:float; mutable xy:float; mutable xz:float;
  mutable yx:float; mutable yy:float; mutable yz:float;
  mutable zx:float; mutable zy:float; mutable zz:float};;
  
(* let unit = {xx = 1.; xy = 0.; xz = 0.; yx = 0.; yy = 1.; yz = 0.; zx = 0.; zy = 0.; zz = 1.} in*)
let unit = [|[|1.; 0.; 0.|]; [|0.; 1.; 0.|]; [|0.; 0.; 1.|]|]

class cSimpleMatrix3D =
 object(self : 's)

  val mMatrix = unit

  method translate x y =
    self#multiply [|[|1.; 0.; 0.|]; [|0.; 1.; 0.|]; [|x; y; 1.|]|]

  method coords x y =
    {x = x *. mMatrix.(0).(0) +. y *. mMatrix.(1).(0) +. mMatrix.(2).(0);
     y = x *. mMatrix.(0).(1) +. y *. mMatrix.(1).(1) +. mMatrix.(2).(1)}

  method rotate rot =
    let c = cos rot and s = sin rot in
      self#multiply [|[|c; s; 0.|]; [|-.s; c; 0.|]; [|0.; 0.; 1.|]|]

  method scale x y =
    self#multiply [|[|x; 0.; 0.|]; [|0.; y; 0.|]; [|0.; 0.; 1.|]|]

  method identity = unit

  method multiply matrix =
    for x = 0 to 2 do
      for y = 0 to 2 do
        let sum = ref 0. in
        for z = 0 to 2 do
          sum := !sum +. matrix.(x).(z) *. mMatrix.(z).(y)
        done;
        mMatrix.(x).(y) <- !sum;
      done;
    done

end;;

