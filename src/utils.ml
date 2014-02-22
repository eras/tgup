module Matrix =
struct
  include Gg
  include M3
  let ( *| ) a b = mul a b
  let ( *|| ) a b = mul b a
end
