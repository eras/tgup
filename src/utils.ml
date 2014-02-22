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
