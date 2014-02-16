open Batteries
open Gtk

let pi2 = 8. *. atan 1.

let destroy () =
  GMain.Main.quit ()

let ba_of_string str =
  let open Bigarray in
  let open Array1 in
  let ba = create int8_unsigned c_layout (String.length str) in
  for c = 0 to String.length str - 1 do
    ba.{c} <- Char.code str.[c]
  done;
  ba

let flip_y (a, b) = (a, ~-. b)

let angle_of_matrix m3 = Gg.(V2.angle (V2.tr m3 (V2.v 1.0 0.0)))

let add_calibration_point env (cam_xy, cnc_xy) =
  env#points := (cam_xy, cnc_xy)::!(env#points);
  ( match !(env#points) with
  | (xy1, cnc_xy1)::_::_ ->
    let open Gg in
    let (xy2, cnc_xy2) = List.hd (List.rev !(env#points)) in
    let dcam_xy = V2.sub xy2 xy1 in
    let dcnc_xy = V2.sub cnc_xy1 cnc_xy2 in (* consider cnc movement to the opposing direction negative *)
    Printf.printf "image point: (%f,%f)\n" (V2.x dcam_xy) (V2.y dcam_xy);
    Printf.printf "cnc point: (%f,%f)\n" (V2.x dcnc_xy) (V2.y dcnc_xy);
    let angle = V2.angle dcnc_xy -. V2.angle dcam_xy in
    Printf.printf "Angle: %f\n%!" (angle /. pi2 *. 360.0);
    let scale' = V2.norm dcnc_xy /. V2.norm dcam_xy in
    Printf.printf "Scale: %f\n%!" scale';

    let open M3 in
    let m = id in
    let m = mul m (rot angle) in
    let m = mul m (scale2 (V2.v scale' scale')) in
    env#liveview#set_angle (angle_of_matrix m);
    Printf.printf "camera_to_world: %s\n%!" (M3.to_string m);
    env#camera_to_world := Some m;
  | _ -> ()
  )

let show_location env cam_xy =
  match !(env#camera_to_world) with
  | None -> ()
  | Some camera_to_world ->
    let open Gg in
    let cam_in_world = V2.tr camera_to_world cam_xy in
    env#info#set_label (Printf.sprintf "%s\n%s" (V2.to_string cam_xy) (V2.to_string cam_in_world))

let image_updater env =
  let io_watch = ref None in
  let frames = ref 0 in
  let t0 = Unix.gettimeofday () in
  fun () ->
    let rec wait_io () = 
      io_watch := Some (
	GMain.Io.add_watch
          ~cond:[`IN]
          ~callback:update_image
          (GMain.Io.channel_of_descr (V4l2.get_fd env#video))
      )
    and unwait_io () =
      match !io_watch with
      | None -> ()
      | Some id ->
	GMain.Io.remove id;
	io_watch := None
    and update_image conditions =
      unwait_io ();
      let id = ref None in
      id := 
	Some ( 
	  GMain.Idle.add @@ fun () ->
	    GMain.Idle.remove (Option.get !id);
	    let frame = V4l2.get_frame env#video in
	    let rgb = frame#rgb_ba in
	    let now = Unix.gettimeofday () in
	    incr frames;
	    Printf.printf "%d %.2f  \r%!" !frames (float !frames /. (now -. t0));
	    env#liveview#set_image ((640, 480), rgb);
	    wait_io ();
	    false 
	);
      true
    in
    V4l2.start env#video;
    ignore (update_image [])

let cnc_moved env (x_ofs, y_ofs) =
  match !(env#camera_to_world) with
  | None -> ()
  | Some camera_to_world ->
    let open Gg in
    let ( * ) = M3.mul in
    let ( *| ) a b = M3.mul b a in
    let world_to_camera = M3.inv camera_to_world in
    Printf.printf "Moved by %f, %f\n%!" x_ofs y_ofs;
    let cnc_movement = M3.move (V2.v ~-.x_ofs ~-.y_ofs) in
    Printf.printf "Matrix: %s\n%!" (M3.to_string cnc_movement);
    Printf.printf "Translation of 0.0: %s\n%!" (M3.to_string cnc_movement);
    let orig = camera_to_world * !(env#point_mapping) in
    env#point_mapping := world_to_camera * ((M3.scale2 @@ V2.v 1.0 1.0) *| cnc_movement) * orig;
    Printf.printf "Point mapping: %s\n%!" (M3.to_string !(env#point_mapping))

let draw_overlay env cairo =
  let open Cairo in
  set_source_rgba cairo 0.0 1.0 0.0 0.5;
  arc cairo 0.0 0.0 20.0 0.0 pi2;
  fill cairo;
  set_source_rgba cairo 1.0 0.0 0.0 0.5;
  flip List.iter !(env#points) @@ fun ((xy), _) ->
    let (x, y) = Gg.(V2.to_tuple @@ P2.tr !(env#point_mapping) xy) in
      (* Printf.printf "Resulting point: %f, %f\n%!" x y; *)
    arc cairo x y 10.0 0.0 pi2;
    fill cairo

let move_cnc env cam_xy camera_to_world =
  (* Move the most recently clicked point over the center *)
  let open Gg in
  let move = P2.tr camera_to_world cam_xy in
  env#cnc_control#adjust_position (V2.x move) (V2.y move)

let save_location var at = 
  var := Some at

let save_location_difference var at0 at1 = 
  match at0 with
  | None -> None
  | Some at0 ->
    let d = Gg.V3.sub at0 at1 in
    var := Some d;
    Some d

let location_label at =
  let open Gg.V3 in
  Printf.sprintf "X:%.3f Y:%.3f Z:%.3f" (x at) (y at) (z at)

let mark_location_widget ~label ~packing ?tooltip cnc f =
  let tooltips = GData.tooltips () in
  let mark_box = GPack.hbox ~packing () in
  let mark_button = GButton.button ~label ~packing:mark_box#pack () in
  ( match tooltip with 
  | None -> ()
  | Some text -> tooltips#set_tip mark_button#coerce ~text);
  let mark_label = GMisc.label ~packing:mark_box#pack () in
  mark_label#set_label "Unset";
  let set_mark event =
    let status = Cnc.wait cnc Cnc.status_tinyg in
    mark_label#set_label (f (Gg.V3.v status.x status.y status.z));
  in
  ignore (mark_button#connect#clicked ~callback:set_mark)

let get_cnc_position cnc =
  let status = Cnc.wait cnc Cnc.status_tinyg in
  Gg.V2.v status.x status.y

let (@.) f g x = g (f x)

let gui sigint_triggered config camera_matrix_arg =
  let video = V4l2.init "/dev/video0" { width = 640; height = 480 } in

  let main_window = GWindow.window ~border_width:10 () in
  Gobject.set GtkBaseProps.Window.P.allow_shrink main_window#as_window true;
  ignore (main_window#connect#destroy ~callback:destroy);
  ignore (sigint_triggered#add_persistent_callback (fun () -> destroy ()));
  let vbox = GPack.vbox ~packing:main_window#add () in
  let quit_button = GButton.button ~label:"Quit" ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~fill:true ~padding:5) () in
  let hbox = GPack.hbox ~packing:vbox#add () in
  ignore (quit_button#connect#clicked ~callback:destroy);
  let camera_to_world = ref camera_matrix_arg in
  let liveview = LiveView.view ~angle:(Option.map_default angle_of_matrix 0.0 !camera_to_world) ~packing:hbox#add (640, 480) () in
  let cnc = Cnc.connect config.Common.co_device config.Common.co_bps in
  let control_box = GPack.vbox ~packing:(hbox#pack ~expand:false ~padding:5) () in
  let cnc_control = CncControl.view ~packing:(control_box#pack) cnc () in
  let info = GMisc.label ~packing:control_box#pack () in
  let int_of_coord_mode = function
  | `CoordModeCNC -> 0
  | `CoordModeCamera -> 1
  in
  let coord_mode_of_int = function
  | 0 -> `CoordModeCNC
  | 1 -> `CoordModeCamera
  | _ -> assert false
  in
  let current_coord_mode = ref `CoordModeCNC in
  let points = ref [] in
  let mark_cnc_location : Gg.V3.t option ref = ref None in
  let mark_camera_offset : Gg.V3.t option ref = ref None in
  let (coord_mode_selection, _) = GEdit.combo_box_text ~strings:["CNC Mode"; "Camera mode"] ~active:(int_of_coord_mode !current_coord_mode) ~packing:control_box#pack () in
  let point_mapping = ref Gg.M3.id in
  let env = object
    method points	   = points
    method point_mapping   = point_mapping
    method liveview	   = liveview
    method camera_to_world = camera_to_world
    method info		   = info
    method video	   = video
    method cnc_control	   = cnc_control
  end in
  let set_coord_mode coord_mode =
    ( match !current_coord_mode, coord_mode, !mark_camera_offset with 
    | `CoordModeCNC, `CoordModeCamera, Some offset ->
      current_coord_mode := `CoordModeCamera;
      let offset' = Gg.V3.neg offset in
      env#cnc_control#adjust_position (Gg.V3.x offset') (Gg.V3.y offset')
    | `CoordModeCamera, `CoordModeCNC, Some offset ->
      current_coord_mode := `CoordModeCNC;
      env#cnc_control#adjust_position (Gg.V3.x offset) (Gg.V3.y offset)
    | _ -> 
      () );
    coord_mode_selection#set_active (int_of_coord_mode !current_coord_mode)
  in
  ignore (coord_mode_selection#connect#changed (fun () ->
    set_coord_mode (coord_mode_of_int coord_mode_selection#active)
  ));
  let _ = mark_location_widget ~label:"Mark" ~tooltip:"Mark the current position of drill" cnc (tap (save_location mark_cnc_location) @. location_label) ~packing:control_box#pack in
  let set_camera_offset at =
    match save_location_difference mark_camera_offset !mark_cnc_location at with
    | None -> ""
    | Some ofs ->
      ( match !current_coord_mode with
      | `CoordModeCNC ->
	current_coord_mode := `CoordModeCamera;
	coord_mode_selection#set_active (int_of_coord_mode !current_coord_mode)
      | _ -> () );
      location_label ofs
  in
  let _ = mark_location_widget ~label:"Camera\noffset" ~tooltip:"Measure distance between camera and drill mark" cnc set_camera_offset ~packing:control_box#pack in
  ignore (Hook.hook liveview#overlay (draw_overlay env));
  ignore (Hook.hook cnc_control#position_adjust_callback (cnc_moved env));
  cnc_moved env cnc_control#get_position;
  ignore (Hook.hook liveview#on_button_press (fun cam_xy ->
    Printf.printf "Clicked at %s\n%!" (Gg.V2.to_string cam_xy);
    match !camera_to_world with
    | None -> add_calibration_point env (cam_xy, get_cnc_position cnc);
    | Some camera_to_world-> move_cnc env cam_xy camera_to_world
  ));
  ignore (Hook.hook liveview#on_mouse_move (show_location env));
  image_updater env ();
  main_window#show ();
  GMain.Main.main ()

let align = gui
