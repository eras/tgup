open Gtk

type coord_mode = 
| CoordModeTool
| CoordModeCamera

let view ~packing cnc () =
  let with_cnc f =
    match cnc with
    | None -> ()
    | Some cnc -> f cnc
  in
  let vbox = GPack.vbox ~packing () in
  let directionals_alignment = GBin.alignment ~packing:vbox#pack ~padding:(10, 10, 50, 50) ~xalign:1.0 () in
  let directionals = GPack.table ~packing:directionals_alignment#add ~columns:3 ~rows:3 () in
  let (travel_length, _) = GEdit.combo_box_entry_text ~strings:["0.05"; "0.1"; "1.0"; "10.0"; "20.0"] ~packing:vbox#pack () in
  let info = GMisc.label ~packing:(vbox#pack ~expand:true) () in
  travel_length#entry#set_text "1.0";
  let cnc_x = ref 0.0  in
  let cnc_y = ref 0.0 in
  let position_adjust_callback = Hook.create () in
  let coord_mode = ref CoordModeTool in
  let camera_offset = ref (Gg.V2.v 0.0 0.0) in
  let coord_mode_offset = function
    | CoordModeTool -> Gg.V2.v 0.0 0.0
    | CoordModeCamera -> !camera_offset
  in
  let move_by x_ofs y_ofs =
    cnc_x := !cnc_x +. x_ofs;
    cnc_y := !cnc_y +. y_ofs;
    Hook.issue position_adjust_callback (Gg.V2.v x_ofs y_ofs);
    assert (abs_float x_ofs < 5.0);
    assert (abs_float y_ofs < 5.0);
    with_cnc @@ fun cnc ->
      Cnc.wait cnc (Cnc.set_feed_rate 100.0);
      Cnc.wait cnc Cnc.set_relative;
      Cnc.ignore cnc (Cnc.travel [`X x_ofs; `Y y_ofs])
  in
  let move x_dir y_dir _ =
    let length = float_of_string (travel_length#entry#text) in
    let x_ofs = float x_dir *. length in
    let y_ofs = float y_dir *. length in
    move_by x_ofs y_ofs
  in
  ignore ((GButton.button ~label:"Y+" ~packing:(directionals#attach ~left:1 ~top:0) ())#connect#clicked (move (0) (1)));
  ignore ((GButton.button ~label:"Y-" ~packing:(directionals#attach ~left:1 ~top:2) ())#connect#clicked (move (0) (-1)));
  ignore ((GButton.button ~label:"X+" ~packing:(directionals#attach ~left:2 ~top:1) ())#connect#clicked (move (1) (0)));
  ignore ((GButton.button ~label:"X-" ~packing:(directionals#attach ~left:0 ~top:1) ())#connect#clicked (move (-1) (0)));
  let handle_tinyg_report (report : Cnc.status_tinyg) = info#set_label (Printf.sprintf "CNC: X%.3f Y%.3f Z%.3f" report.x report.y report.z) in
  with_cnc (fun cnc ->
    ignore (Hook.hook (Cnc.status_report_tinyg cnc) handle_tinyg_report);
    let status = Cnc.wait cnc Cnc.status_tinyg in
    cnc_x := status.x;
    cnc_y := status.y;
    handle_tinyg_report status
  );
  let adjust_coord_mode f =
    let coord_ofs0 = coord_mode_offset !coord_mode in
    f ();
    let coord_ofs1 = coord_mode_offset !coord_mode in
    let coord_delta = Gg.V2.sub coord_ofs1 coord_ofs0 in
    let (x_ofs, y_ofs) = Gg.V2.to_tuple coord_delta in
      with_cnc @@ fun cnc ->
	Cnc.wait cnc (Cnc.set_feed_rate 100.0);
	Cnc.wait cnc Cnc.set_relative;
	Cnc.ignore cnc (Cnc.travel [`X x_ofs; `Y y_ofs])
  in
  object 
    method get_position =
      Gg.V2.sub (Gg.V2.v !cnc_x !cnc_y) (coord_mode_offset !coord_mode)
    method adjust_position xy = move_by (Gg.V2.x xy) (Gg.V2.y xy)
    method position_adjust_callback = position_adjust_callback

    method get_coord_mode = !coord_mode
    method set_coord_mode coord_mode' =
      adjust_coord_mode @@ fun () ->
	coord_mode := coord_mode'
    method set_camera_offset offset =
      adjust_coord_mode @@ fun () ->
	camera_offset := offset
  end
