(* some general useful functions / notations independent of maths *)

(* 
  we use invalid_arg to signal a domain error
  in some more general functions
  do not invoke exceptions yourself and 
    make sure it never can get thrown if using those functions
  (this also applies to std functions like Option.get)
*)
(* composition (there really should be a standard notation) *)
let (<<) f g x = f(g(x))
(* std functions for conversion between character lists and strings *)
let implode = String.of_seq << List.to_seq


(* some useful math functions categorized in general, 2d, 3d, nd *)
(* you will not need most functions (especially from 2d and 3d) but it makes things easier
    to have a large arsenal of common math functions available (especially vector and matrix functions) *)


(* General *)

let rec iter f a = function
    | 0 -> a
    | n -> iter f (f a) (Int.sub n 1)
let car2cas f x y = f (x,y)
let cas2car f (x,y) = f x y
let triple x = (x,x,x)
let range n = List.init n (fun i -> Float.of_int i /. (Float.of_int n -. 1.))
let group n xs = 
  let rec aux acc k xs = 
    if k = 0 then (List.rev acc)::(aux [] k xs)
    else match xs with
      | [] -> [List.rev acc]
      | x::xs -> aux (x::acc) (k-1) xs
  in aux [] n xs

(* Float functions *)
(* haskells notation and $ would be helpful *)
let clampBound a b x : float = min (max x a) b
let clamp = clampBound 0.0 1.0
let log x = if x<=0.0 then 0.0 else Float.log x
(* modulus for float *)
let modR (x,y) : float = x-.y*.Float.floor(x/.y)
let sq x = x *. x
let mix a b x = x*.b+.(1.0-.x)*.a
let lerp = mix
let abs=Float.abs
let pow=Float.pow
let sign x = if x < 0.0 then ~-. 1.0 else (if x > 0.0 then 1.0 else 0.0)
let rad_of_degree deg = deg*.Float.pi/.180.
let pi = Float.pi

(* 2d *)

(* we use pairs instead of more general lists for easier pattern matching *)
let map_pair f (x,y) = (f x, f y)
let vec2 = map_pair
let clampP = map_pair clamp
let length2 (x,y) :float = sqrt(x*.x+.y*.y)
let zip2 (ax,ay) (bx,by) = ((ax,bx),(ay,by))
let vec2Bin f x y = vec2 f (zip2 x y)
let matmul42 (a,b,c,d) (x,y) :float*float = (a*.x+.y*.b,x*.c+.y*.d)
let matmul24 (x,y) (a,b,c,d) :float*float = (a*.x+.c*.y,b*.x+.d*.y)
let scale2 (s:float) = vec2 (fun x -> s*.x)
let dot2 (ax,ay) (bx,by) :float = ax*.bx+.ay*.by
let mag2 p = dot2 p p
let sub2 = vec2Bin (cas2car (-.))
let abs2 = vec2 abs
let lerp2 v1 v2 t = vec2Bin (fun (a,b) -> lerp a b t) v1 v2

(* 3d *)

let map_triple f (x,y,z) = (f x, f y, f z)
let vec3 f x = map_triple f x
let swap f x y = f y x
let mag3 (x,y,z) = x*.x+.y*.y+.z*.z
let length3 p = sqrt(mag3 p)
let normalize p = vec3 (fun x -> x /. length3 p) p
let cross (ax,ay,az) (bx,by,bz) = 
        (ay*.bz-.az*.by, 
         az*.bx-.ax*.bz, 
         ax*.by-.ay*.bx)
let zip3 (ax,ay,az) (bx,by,bz) = ((ax,bx),(ay,by),(az,bz))
let vecX = (1.0,0.0,0.0)
let vecY = (0.0,1.0,0.0)
let vecZ = (0.0,0.0,1.0)
let vecBin f x y = vec3 f (zip3 x y)
let sub = vecBin (cas2car (-.))
let add = vecBin (cas2car (+.))
let scale3 s = vec3 (fun x -> s*.x)
let abs3 = vec3 abs
let dot (ax,ay,az) (bx,by,bz) = ax*.bx+.ay*.by+.az*.bz (* sum of vecBin op* *)
let smoothstep a b x = 
    let y = clamp ((x-.a)/.(b-.a)) in
    y*.y*.(3.-.2.*.y)
let unit_cube=((-1.0,-1.0,-1.0),(1.0,1.0,1.0))
let vec_mix a b x = vecBin (fun (a,b) -> mix a b x) a b


(* n-dimensional *)
let sum = List.fold_left (+.) 0.0
let mag xs :float = sum (List.map sq xs)
let length p = sqrt(mag p)
let scale (min,max) s x = (float_of_int x /. float_of_int s)*.(max-.min)+.min
let scalef (min,max) s x = (x /. s)*.(max-.min)+.min
let zip = List.combine
let subV p q : float list = List.map (cas2car (-.)) (zip p q)
;;
let to_list2 (x,y) = [x;y]
let to_list3 (x,y,z) = [x;y;z]
let to_tuple2 = function
    | [x;y] -> (x,y)
    | _ -> invalid_arg "2d conversion to tuple"
let to_tuple3 = function
    | [x;y;z] -> (x,y,z)
    | _ -> invalid_arg "3d conversion to tuple"


(* completely unrelated ;) 
   a smooth minimum function *)
let smin k a b = 
    let h = clamp (0.5*.(b-.a)/.k+.0.5) in 
        mix b a h -. k*.h*.(1.0-.h)

let bezier' a b c d t =
    let pAB = lerp2 a b t in
    let pBC = lerp2 b c t in
    let pCD = lerp2 c d t in
    
    let pABC = lerp2 pAB pBC t in
    let pBCD = lerp2 pBC pCD t in
    
    let pABCD = lerp2 pABC pBCD t in
    (pABCD,(pABC,pBCD),(pAB,pBC,pCD))
    
let bezier a b c d t =
    let (p,_,_) = bezier' a b c d t in p


let color (a,b,c,d) t =
    let ab = zip3 a b in
    let cd = zip3 c d in
    let abcd = zip3 ab cd in
    map_triple (fun ((a,b),(c,d)) ->
        a+. b*.cos(6.28318*.c*.t+.d)
    ) abcd

let white_blue_red = color (
    (0.50,0.50,0.50),
    (0.50,0.50,0.50),
    (1.00,0.70,0.40),
    (0.00,0.15,0.20)
)

let blue_pink_blue = color (
    (0.50,0.50,0.50),
    (0.50,0.50,0.50),
    (2.00,1.00,0.00),
    (0.50,0.20,0.25)
)

let brown_blue = color (
    (0.50,0.50,0.50),
    (0.50,0.50,0.50),
    (1.00,1.00,1.00),
    (0.00,0.33,0.67)
)

let blue_yellow = color (
    (0.50,0.50,0.50),
    (0.50,0.50,0.50),
    (1.00,1.00,0.50),
    (0.80,0.90,0.30)
)

(* use modf for infinite repeating scenes (try to figure out how) *)

let m2 = (0.8,0.6,-0.6,0.8)
let third (_,_,c) = c
(* 
    a few different noise texture suitable for textures 
    perlin noise is another one
*)
let noise p =
    0.25*.(third(iter (fun (p,f,r) -> 
        let (x',y') = vec2 (fun x -> f*.x+.0.6) (matmul42 m2 p) in
        ((x',y'),f,r+.sin(x'+.sin(2.0*.y'))) 
    ) (p,2.0,0.0) 4))
let fbmabs p =
    third(iter (fun (p,f,r) -> 
        (vec2Bin (cas2car (-.)) p (-0.01*.r,0.08*.r), 2.0*.f, r+.abs(noise(scale2 f p))/.f)
    ) (p,1.0,0.0) 8)
let fbmstars p =
    pow (third(iter (fun (p,f,r) -> 
        (matmul24 p m2, f+.1.0, r+.noise(scale2 (20.0+.3.0*.f) p)/.f)
    ) (vec2 (fun x -> Float.floor(50.0*.x)/.50.0) p,1.0,0.0) 5)) 8.0
let fbmdisk p =
    1.0/.(third(iter (fun (p,f,r) -> 
        (p, f+.1.0, r+.abs(noise(scale2 f p))/.f)
    ) (p,1.0,0.0) 7))
let fbmdust p =
    pow (1.0-.1.0/.(third(iter (fun (p,f,r) -> 
        (p, f+.1.0, r+.1.0/.abs(noise(scale2 f p))/.f)
    ) (p,1.0,0.0) 7))) 4.0
let bulb p = 
    exp(-1.2*.mag2 p) +. 0.5*.exp(-12.0*.mag2 (vec2Bin (cas2car (-.)) p (0.0,-0.2)))