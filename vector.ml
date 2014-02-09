type side = Left | Right;;

let pi = atan (1.0) *. 4.0;;

let inv2x2 m =
  match (m.(0).(0), m.(0).(1), m.(1).(0), m.(1).(1)) with (a, b, c, d) ->
    let mult = (1.0 /. (a *. d -. b *. c)) in
    [| [| mult *. d;		mult *. (-. b) |];
       [| mult *. (-. c);	mult *. a |] |]

let base2x2 vec =
  match vec with (x, y) ->
    [| [| x; y |];
       [| -. y ; x |] |];;

let mul2x2 m1 m2 =
  [| [| m1.(0).(0) *. m2.(0).(0) +. m1.(0).(1) *. m2.(1).(0);
	m1.(0).(0) *. m2.(0).(1) +. m1.(0).(1) *. m2.(1).(1); |];
     [| m1.(1).(0) *. m2.(0).(0) +. m1.(1).(1) *. m2.(1).(0);
	m1.(1).(0) *. m2.(0).(1) +. m1.(1).(1) *. m2.(1).(1); |]; |];;

let dot2 (x1, y1) (x2, y2) =
  (x1 *. x2) +. (y1 *. y2);;

let transform v m2 =
  match v with (x, y) ->
    ( x *. m2.(0).(0) +. y *. m2.(1).(0),
      x *. m2.(0).(1) +. y *. m2.(1).(1) );;

let rot90 (x, y) = 
  (-. y, x);;

let add_vector v1 v2 =
  match (v1, v2) with
    ((x1, y1), (x2, y2)) -> ((x1 +. x2), (y1 +. y2));;

let sub_vector v1 v2 =
  match (v1, v2) with
    ((x1, y1), (x2, y2)) -> ((x1 -. x2), (y1 -. y2));;

let neg_vector (a, b) = (-.a, -.b)

let mul_vector_float (a, b) x = (a *. x, b *. x)

let mul_vector (x1, y1) (x2, y2) = (x1 *. x2, y1 *. y2)

let mul_float_vector x (a, b) = (x *. a, x *. b)

let div_vector_float (a, b) x = (a /. x, b /. x)

module Ops = struct
  let ( +| ) = add_vector
  let ( -| ) = sub_vector
  let ( ~| ) = neg_vector
  let ( *| ) = mul_vector
  let ( *|. ) = mul_vector_float
  let ( *.| ) = mul_float_vector 
  let ( /|. ) = div_vector_float 
end

let point_side (a, b) p =
  let p1 = sub_vector b a in
  let p2 = sub_vector p a in
  let v = dot2 (rot90 p1) p2 in
  if v > 0.0 then
    Left
  else
    Right;;

let unit (x, y) =
  let l = sqrt ( x *. x +. y *. y ) in
  (x /. l, y /. l);;

let point_distance (a, b) p =
  let p1 = unit (sub_vector b a) in
  let p2 = sub_vector p a in
  dot2 (rot90 p1) p2;;

(* creates a transformation function that maps pixels so that 
   (f (fst segment), f (snd segment)) = (fst base, snd base) *)
let gen_transform base segment =
  let (base_vertex, base_delta, vertex, delta) =
    match base, segment with
      (base, base2), 
      (v1, v2 )->
	(base,
	 sub_vector base2 base,
	 v1, 
	 sub_vector v2 v1) in
  let m = mul2x2 (inv2x2 (base2x2 base_delta)) (base2x2 delta) in
  let m' = inv2x2 m in
  let f vec = (add_vector 
		 base_vertex
		 (transform 
		    (sub_vector vec vertex) 
		    m'
		 )
	      ) in
  let f' vec = (add_vector 
		  vertex
		  (transform 
		     (sub_vector vec base_vertex) 
		     m
		  )
	       ) in
  (f, f');;

let angle (vx, vy) =
  let t = if (vx > vy) then abs_float ( atan (vy /. vx) ) else pi /. 2.0 -. abs_float ( atan (vx /. vy) ) in
  match (vx >= 0.0, vy >= 0.0) with
    (false, false) -> pi +. t
  | (true,  false) -> 2.0 *. pi -. t
  | (false, true)  -> pi -. t
  | (true,  true)  -> t
;;

let rotation angle =
  [| [| cos angle;      sin angle |];
     [| -. (sin angle); cos angle|] |];;

let transform (x, y) m =
  (x *. m.(0).(0) +. y *. m.(0).(1),
   x *. m.(1).(0) +. y *. m.(1).(1));;

let base_transform (base_x, base_y) m (x, y) =
  let (x', y') = transform (float_of_int (x - base_x), float_of_int (y - base_y)) m in
  ((int_of_float x') + base_x, (int_of_float y') + base_y);;

let scale_vector scale (x, y) =
  (x *. scale, y *. scale);;

let length (x, y) =
  sqrt ( x *. x +. y *. y );;

let sign = function
  | n when n < 0 -> -1
  | n -> 1

(* lazy, got the formulas from http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/ *)
let intersect_u ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  let dem = ((y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)) in
    if dem = 0 then
      None
    else
      let u'a = float ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) /. float dem in
      let u'b = float ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) /. float dem in
	Some ((u'a, lazy ((x1 + int_of_float (u'a *. float (x2 - x1))),
			  (y1 + int_of_float (u'a *. float (y2 - y1))))), 
	      (u'b, lazy ((x3 + int_of_float (u'b *. float (x4 - x3))),
			  (y3 + int_of_float (u'b *. float (y4 - y3))))))

let intersect_u_float ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  let dem = ((y4 -. y3) *. (x2 -. x1) -. (x4 -. x3) *. (y2 -. y1)) in
    if dem = 0.0 then
      None
    else
      let u'a = ((x4 -. x3) *. (y1 -. y3) -. (y4 -. y3) *. (x1 -. x3)) /. dem in
      let u'b = ((x2 -. x1) *. (y1 -. y3) -. (y2 -. y1) *. (x1 -. x3)) /. dem in
	Some ((u'a, lazy ((x1 +. (u'a *. (x2 -. x1))),
			  (y1 +. (u'a *. (y2 -. y1))))),
	      (u'b, lazy ((x3 +. (u'b *. (x4 -. x3))),
			  (y3 +. (u'b *. (y4 -. y3))))))

let intersect_line a b =
  match intersect_u a b with
    | None -> None
    | Some ((u'a, p), (u'b, _)) 
	when u'a >= 0.0 && u'a <= 1.0 && u'b >= 0.0 && u'b <= 1.0 ->
	Some (Lazy.force p)
    | _ -> None

(* the line a is considered of being infinitely long *)
let intersect_line_any a b =
  match intersect_u a b with
    | None -> None
    | Some ((u'a, p), (u'b, _)) 
	when u'b >= 0.0 && u'b <= 1.0 ->
	Some (Lazy.force p)
    | _ -> None

(* lazy, got the formulas from http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/ *)
let intersect_line_float ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  let dem = ((y4 -. y3) *. (x2 -. x1) -. (x4 -. x3) *. (y2 -. y1)) in
    if dem = 0.0 then
      None
    else
      let u'a = ((x4 -. x3) *. (y1 -. y3) -. (y4 -. y3) *. (x1 -. x3)) /. dem in
      let u'b = ((x2 -. x1) *. (y1 -. y3) -. (y2 -. y1) *. (x1 -. x3)) /. dem in
	if u'a >= 0.0 && u'a <= 1.0 && u'b >= 0.0 && u'b <= 1.0 then
	  Some ((x1 +. u'a *. (x2 -. x1)),
		(y1 +. u'a *. (y2 -. y1)))
	else
	  None

(* returns intersecting points in the order of intersection *)
let intersect_region_with intersector ((x1, y1), (x2, y2)) ((p, _) as line) =
  let d2 (x1, y1) = 
    let (x2, y2) = p in
      (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
  in
  List.sort 
    (fun p1 p2 -> compare (d2 p1) (d2 p2))
    (List.concat (
       (List.map (fun border -> 
		    match intersector border line with
		      | None -> []
		      | Some x -> [x])
	  [(x1, y1), (x1, y2 - 1);
	   (x1, y2 - 1), (x2 - 1, y2 - 1); 
	   (x2 - 1, y2 - 1), (x2 - 1, y1);
	   (x2 - 1, y1), (x1, y1)])
     )
    )

let intersect_region = intersect_region_with intersect_line

let intersect_borders = intersect_region_with intersect_line_any
