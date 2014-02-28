open Gtk

type coord_mode = 
| CoordModeTool
| CoordModeCamera

let movement_limit = false

let view ~packing cnc () =
  let with_cnc f =
    match cnc with
    | None -> ()
    | Some cnc -> f cnc
  in
  let vbox = GPack.vbox ~packing () in
  let directionals_alignment = GBin.alignment ~packing:vbox#pack ~padding:(10, 10, 50, 50) ~xalign:1.0 () in
  let directionals_hbox = GPack.hbox ~packing:directionals_alignment#add () in
  let directionals =
    let frame = GBin.frame ~packing:directionals_hbox#add () in
    GPack.table ~packing:frame#add ~columns:5 ~rows:4 () in
  let z_controls_box =
    let frame = GBin.frame ~packing:directionals_hbox#add () in
    GPack.table ~packing:frame#add ~columns:3 ~rows:4 () in
  let (travel_length, _) = GEdit.combo_box_entry_text ~strings:["0.05"; "0.1"; "1.0"; "10.0"; "20.0"] ~packing:(directionals#attach ~left:0 ~right:3 ~top:3) () in
  let (z_travel_length, _) = GEdit.combo_box_entry_text ~strings:["0.05"; "0.1"; "1.0"] ~packing:(z_controls_box#attach ~left:0 ~top:3) () in
  let info = GMisc.label ~packing:(vbox#pack ~expand:true) () in
  travel_length#entry#set_text "1.0";
  z_travel_length#entry#set_text "1.0";
  let cnc_x = ref 0.0 in
  let cnc_y = ref 0.0 in
  let cnc_z = ref 0.0 in
  let reference_z = ref 0.0 in          (* virtual zero-level *)
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
    Hook.issue position_adjust_callback (Gg.V3.v x_ofs y_ofs 0.0);
    with_cnc @@ fun cnc ->
      if movement_limit then (
        assert (abs_float x_ofs < 5.0);
        assert (abs_float y_ofs < 5.0);
      );
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
  let move_z_by z_ofs =
    cnc_z := !cnc_z +. z_ofs;
    Hook.issue position_adjust_callback (Gg.V3.v 0.0 0.0 z_ofs);
    with_cnc @@ fun cnc ->
      Cnc.wait cnc (Cnc.set_feed_rate 100.0);
      Cnc.wait cnc Cnc.set_relative;
      Cnc.ignore cnc (Cnc.travel [`Z z_ofs])
  in
  let move_z z_dir _ =
    let length = float_of_string (z_travel_length#entry#text) in
    let z_ofs = float z_dir *. length in
    move_z_by z_ofs
  in
  let reset_z _ = reference_z := !cnc_z in
  let elements = ref [] in
  let add e = elements := e::!elements; e in
  ignore ((add @@ GButton.button ~label:"Y+" ~packing:(directionals#attach ~left:1 ~top:0) ())#connect#clicked (move (0) (1)));
  ignore ((add @@ GButton.button ~label:"Y-" ~packing:(directionals#attach ~left:1 ~top:2) ())#connect#clicked (move (0) (-1)));
  ignore ((add @@ GButton.button ~label:"X+" ~packing:(directionals#attach ~left:2 ~top:1) ())#connect#clicked (move (1) (0)));
  ignore ((add @@ GButton.button ~label:"X-" ~packing:(directionals#attach ~left:0 ~top:1) ())#connect#clicked (move (-1) (0)));
  ignore ((add @@ GButton.button ~label:"Z+" ~packing:(z_controls_box#attach ~left:0 ~top:0) ())#connect#clicked (move_z (1)));
  ignore ((add @@ GButton.button ~label:"Z-" ~packing:(z_controls_box#attach ~left:0 ~top:1) ())#connect#clicked (move_z (-1)));
  ignore ((add @@ GButton.button ~label:"Reset Z" ~packing:(z_controls_box#attach ~left:0 ~top:2) ())#connect#clicked reset_z);
  let handle_tinyg_report (report : Cnc.status_tinyg) = 
    info#set_label (
      let (x, y) = Gg.V2.to_tuple (Gg.V2.sub (Gg.V2.v report.x report.y) (coord_mode_offset !coord_mode)) in
      Printf.sprintf "CNC: X%.3f Y%.3f Z%.3f" x y report.z
    )
  in
  with_cnc (fun cnc ->
    ignore (Hook.hook (Cnc.status_report_tinyg cnc) handle_tinyg_report);
    let Cnc.ResultOK status = Cnc.wait cnc Cnc.status_tinyg in
    cnc_x := status.x;
    cnc_y := status.y;
    cnc_z := status.z;
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
  let set_enabled mode =
    List.iter 
      (fun el -> el#misc#set_sensitive mode)
      !elements
  in
  let o = object 
    method get_position = Gg.V3.v !cnc_x !cnc_y (!cnc_z -. !reference_z)
    method get_viewport_position = Gg.V2.add (Gg.V2.v !cnc_x !cnc_y) (coord_mode_offset !coord_mode)
    method adjust_position xy = move_by (Gg.V2.x xy) (Gg.V2.y xy)
    method adjust_z z = move_z_by z
    method position_adjust_callback = position_adjust_callback

  (* set position without moving *)
    method set_position v3 = Gg.V3.(cnc_x := x v3; cnc_y := y v3; cnc_z := z v3 +. !reference_z)

    method get_reference_z = !reference_z

    method current_coord_of_position pos = Gg.V2.add pos (coord_mode_offset !coord_mode)
    method get_coord_mode = !coord_mode
    method set_coord_mode coord_mode' =
      adjust_coord_mode @@ fun () ->
        coord_mode := coord_mode'
    method set_camera_offset offset =
      adjust_coord_mode @@ fun () ->
        camera_offset := offset
    method get_camera_offset offset = !camera_offset
    method get_camera_position = Gg.V2.sub (Gg.V2.v !cnc_x !cnc_y) !camera_offset

    method set_enabled = set_enabled
  end in
  let env = object
    method cnc_control = o
  end in
  Widgets.level_store_widget "camera" ~packing:vbox#pack ~env ();
  Widgets.level_store_widget "tool" ~packing:vbox#pack ~env ();
  o
