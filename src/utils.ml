module Matrix =
struct
  include Gg
  include M3
  let ( *| ) a b = mul a b
  let ( *|| ) a b = mul b a

  let cairo_matrix_of_m3 m = 
    let open Cairo in
    let open Gg.M3 in
    {
      xx = e00 m;
      xy = e01 m;
      x0 = e02 m;
      yx = e10 m;
      yy = e11 m;
      y0 = e12 m;
    }

  let m3_of_cairo_matrix m = 
    let open Cairo in
    Gg.M3.v
      m.xx
      m.xy
      m.x0
      m.yx
      m.yy
      m.y0
      0.0
      0.0
      1.0
end

let string_of_tm { Unix.tm_sec = sec;
                   tm_min = min;
                   tm_hour = hour;
                   tm_mday = mday;
                   tm_mon = mon;
                   tm_year = year } =
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (year + 1900)
    (mon + 1)
    (mday)
    (hour)
    (min)
    (sec)

let string_of_time t =
  string_of_tm (Unix.localtime t)

let human_eta seconds =
  let parts = [(60, "s"); (60, "m"); (60, "h"); (24, "d")] in
  let left, segs =
    List.fold_left
      (fun (left, segs) (unit_size, unit_name) ->
	if left > 0
	then
	  (left / unit_size, (Printf.sprintf "%d%s" (left mod unit_size) unit_name :: segs))
	else (left, segs)
      )
      (seconds, [])
      parts
  in
  if left > 0
  then "n/a"
  else String.concat " " segs
      

