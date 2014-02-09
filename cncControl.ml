open Gtk

let view ~packing cnc () =
  let layout = GPack.table ~packing ~columns:3 ~rows:3 () in
  let _ = Cnc.wait cnc (Cnc.set_feed_rate 100.0) in
  let _ = Cnc.wait cnc Cnc.set_relative in
  let move x_dir y_dir _ =
    let x_ofs = float x_dir *. 1.0 in
    let y_ofs = float y_dir *. 1.0 in
    Cnc.ignore cnc (Cnc.travel [`X x_ofs; `Y y_ofs])
  in
  (GButton.button ~label:"Y+" ~packing:(layout#attach ~left:1 ~top:0) ())#connect#clicked (move (0) (1));
  (GButton.button ~label:"Y-" ~packing:(layout#attach ~left:1 ~top:2) ())#connect#clicked (move (0) (-1));
  (GButton.button ~label:"X+" ~packing:(layout#attach ~left:2 ~top:1) ())#connect#clicked (move (1) (0));
  (GButton.button ~label:"X-" ~packing:(layout#attach ~left:0 ~top:1) ())#connect#clicked (move (-1) (0));
  ()

