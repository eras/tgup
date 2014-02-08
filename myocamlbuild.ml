open Ocamlbuild_plugin
open Command

let _ = dispatch begin function
  | After_rules ->
      flag ["ocaml"; "compile"; "no_warn_40"] (S[A"-w"; A"-40"]);
  | _ -> ()
end
