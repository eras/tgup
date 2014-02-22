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

let scale_of_matrix m3 = 
  let open Gg in
  let vec = V2.v 1.0 1.0 in
  V2.norm (V2.tr m3 vec) /. V2.norm vec

let translation_of_matrix m3 = Gg.(P2.tr m3 (V2.v 0.0 0.0))

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
          (GMain.Io.channel_of_descr env#video#get_fd)
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
	    let frame = env#video#get_frame in
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
    env#video#start ();
    ignore (update_image [])

let cnc_moved env (x_ofs, y_ofs) =
  match !(env#camera_to_world) with
  | None -> ()
  | Some camera_to_world ->
    let open Gg in
    let ( *| ) = M3.mul in
    let ( *|| ) a b = M3.mul b a in
    let world_to_camera = M3.inv camera_to_world in
    Printf.printf "Moved by %f, %f\n%!" x_ofs y_ofs;
    let cnc_movement = M3.move (V2.v ~-.x_ofs ~-.y_ofs) in
    Printf.printf "Matrix: %s\n%!" (M3.to_string cnc_movement);
    Printf.printf "Translation of 0.0: %s\n%!" (M3.to_string cnc_movement);
    let orig = camera_to_world *| !(env#point_mapping) in
    env#point_mapping := world_to_camera *| ((M3.scale2 @@ V2.v 1.0 1.0) *|| cnc_movement) *| orig;
    Printf.printf "Point mapping: %s\n%!" (M3.to_string !(env#point_mapping))

let render_gcode cairo mapping_matrix (gcode : Gcode.Parser.word list) =
  let open Cairo in
  let module State = struct
    type at = {
      x : float option;
      y : float option;
      z : float option;
    }
    type t = {
      at   : at
    }
    let init = {
      at = { x = None;
	     y = None;
	     z = None };
    }

    let complete_at at position =
      let module AM = Gcode.Parser.AxisMap in
      let get key default =
	try Some (Gcode.Parser.AxisMap.find key position)
	with Not_found -> default
      in
      {
	x = get `X at.x;
	y = get `Y at.y;
	z = get `Z at.z;
      }
  end in
  let open Gcode.Parser in
  let rec loop (state : State.t) = function
    | [] -> ()
    | command::rest ->
      let state =
	match command with
	| Move ((G0 | G1), position, _) ->
	  ( let _prev = state.at in
	    let state = { State.at = State.complete_at state.at position } in
	    match state.at with
	    | { x = Some x; y = Some y } ->
	      let xy = Gg.V2.v x y in
	      let (x', y') = Gg.(V2.to_tuple @@ P2.tr mapping_matrix xy) in
	      arc cairo x' y' 10.0 0.0 pi2;
	      fill cairo;
	      state
	    | _ -> state )
	| _ -> state
      in
      loop state rest
  in
  set_source_rgba cairo 1.0 0.0 0.0 0.5;
  loop State.init gcode      

let draw_overlay env liveview_context =
  let cairo = liveview_context#cairo in
  let open Cairo in
  set_source_rgba cairo 0.0 1.0 0.0 0.5;
  arc cairo 0.0 0.0 20.0 0.0 pi2;
  fill cairo;
  set_source_rgba cairo 1.0 0.0 0.0 0.5;
  ( flip List.iter !(env#points) @@ fun ((xy), _) ->
    let (x, y) = Gg.(V2.to_tuple @@ P2.tr !(env#point_mapping) xy) in
    (* Printf.printf "Resulting point: %f, %f\n%!" x y; *)
    arc cairo x y 10.0 0.0 pi2;
    fill cairo );
  match !(env#camera_to_world) with
  | None -> ()
  | Some camera_to_world ->
    let world_to_camera = Gg.M3.inv camera_to_world in
    let gcode_to_cnc = Option.default Gg.M3.id !(env#gcode_to_cnc) in
    let ( *| ) = Gg.M3.mul in
    let matrix = !(env#point_mapping) *| (gcode_to_cnc *| world_to_camera) in
    Option.may (render_gcode cairo matrix) !(env#gcode)

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
    match cnc with
    | None -> ()
    | Some cnc ->
      let status = Cnc.wait cnc Cnc.status_tinyg in
      mark_label#set_label (f (Gg.V3.v status.x status.y status.z));
  in
  ignore (mark_button#connect#clicked ~callback:set_mark)

let get_cnc_position cnc =
  let status = Cnc.wait cnc Cnc.status_tinyg in
  Gg.V2.v status.x status.y

let (@.) f g x = g (f x)

let v2_of_v3 v2 =
  let open Gg.V2 in
  Gg.V3.v (x v2) (y v2) 0.0

let v2_of_status_tinyg status = Cnc.(Gg.V2.v status.x status.y)

let coordinate_transformation ~scaled (cnc1, cnc2) (gcode1, gcode2) =
  let open Gg in
  let cnc_delta = V2.sub cnc2 cnc1 in
  let gcode_delta = V2.sub gcode2 gcode1 in
  let translation = V2.sub cnc1 gcode1 in
  let angle = V2.angle cnc_delta -. V2.angle gcode_delta in
  let scale' = V2.norm cnc_delta /. V2.norm gcode_delta in
  let gcode_to_cnc_matrix =
    let open M3 in
    let m = id in
    let ( *| ) = mul in
    let m = 
      if scaled
      then m *| scale2 (V2.v scale' scale') 
      else m in
    let m = m *| rot angle in
    let m = Gg.M3.move (V2.sub cnc1 (V2.tr m gcode1)) *| m in
    m
  in
  gcode_to_cnc_matrix

let alignment_widget ~cnc ~packing gcode_to_cnc_var =
  let tooltips = GData.tooltips () in
  let frame = GBin.frame ~packing ~label:"G-code realignment" ~label_xalign:0.5 () in
  let vbox = GPack.vbox ~packing:frame#add () in
  let results = GMisc.label ~packing:vbox#pack ~selectable:true () in
  let mark_widget ~callback ~label ~tooltip ~packing =
    let mark_box = GPack.hbox ~packing () in
    let mk_mark storage = GEdit.entry ~packing:mark_box#pack ~xalign:1.0 ~width:(10*10) () in
    let mark_ref_x_entry = mk_mark () in
    let mark_ref_y_entry = mk_mark () in
    let mark_button = GButton.button ~label ~packing:mark_box#pack () in
    let validate_entry entry = Pcre.pmatch ~pat:"^[0-9.]+$" entry#text in
    let validate_entries () = validate_entry mark_ref_x_entry && validate_entry mark_ref_y_entry in
    let validate_mark_button_state () =
      mark_button#misc#set_sensitive (validate_entries ());
    in
    ignore (mark_button#connect#clicked (fun () ->
      if validate_entries () then
	let x_ref = float_of_string mark_ref_x_entry#text in
	let y_ref = float_of_string mark_ref_y_entry#text in
	callback (Gg.V2.v x_ref y_ref)
    ));
    validate_mark_button_state ();
    ignore (mark_ref_x_entry#connect#changed validate_mark_button_state);
    ignore (mark_ref_y_entry#connect#changed validate_mark_button_state);
    tooltips#set_tip mark_button#coerce ~text:tooltip
  in
  let mark1 = ref None in
  let mark2 = ref None in
  let mark_callback cur_mark v =
    cur_mark := Some (v, v2_of_status_tinyg @@ Cnc.wait cnc Cnc.status_tinyg);
    match !mark1, !mark2 with
    | Some (gcode1, cnc1), Some (gcode2, cnc2) ->
      let gcode_to_cnc = coordinate_transformation ~scaled:false (cnc1, cnc2) (gcode1, gcode2) in
      gcode_to_cnc_var := Some gcode_to_cnc;
      let _ =
	if false then
	  let open Gg in
          let m = gcode_to_cnc in
          Printf.printf "Point 1 %s translated to cnc: %s (should be %s)\n%!" (V2.to_string gcode1) (V2.to_string (P2.tr m gcode1)) (V2.to_string cnc1);
          Printf.printf "Point 2 %s translated to cnc: %s (should be %s)\n%!" (V2.to_string gcode2) (V2.to_string (P2.tr m gcode2)) (V2.to_string cnc2);
          Printf.printf "Delta 1 %s translated to cnc: %s (should be %s)\n%!" (V2.to_string (V2.sub gcode2 gcode1)) (V2.to_string (V2.tr m (V2.sub gcode2 gcode1))) (V2.to_string (V2.sub cnc2 cnc1));
      in
      let text = 
	let open Gg in
	let angle = angle_of_matrix gcode_to_cnc in
	let scale' = scale_of_matrix gcode_to_cnc in
	let translation = translation_of_matrix gcode_to_cnc in
	Printf.sprintf "angle: %.2f scale : %.3f\ntranslation: %s\n%s"
	  (angle /. Float.pi *. 180.0)
	  scale'
	  (V2.to_string translation)
	  (M3.to_string gcode_to_cnc)
      in
      results#set_text text
    | _ -> ()
  in
  mark_widget ~callback:(mark_callback mark1) ~label:"Mark 1" ~tooltip:"Mark point 1 in work area" ~packing:vbox#pack;
  mark_widget ~callback:(mark_callback mark2) ~label:"Mark 2" ~tooltip:"Mark point 1 in work area" ~packing:vbox#pack;
  ()

let load_button_widget ~packing () =
  let button = GButton.button ~label:"Choose file" ~packing () in
  let filename_chosen_hook = Hook.create () in
  ignore (button#connect#clicked ~callback:(fun () ->
    let file_dialog =
      GWindow.file_chooser_dialog 
	~title:"Open G-code program"
	~modal:true
	~allow_grow:true
	~action:`OPEN ()
    in
    file_dialog#add_select_button_stock `OPEN `OPEN;
    file_dialog#add_button_stock `CANCEL `CANCEL;
    file_dialog#set_filter (GFile.filter ~patterns:["*.gc"; "*.nc"; "*.gcode"] ());
    let result = file_dialog#run () in
    if result = `OPEN then
	match file_dialog#filename with
      | None -> ()
      | Some name ->
	Hook.issue filename_chosen_hook name
    else ();
    file_dialog#destroy ();
  ));
  let obj = object
      method chosen = filename_chosen_hook
  end in 
  obj

let load_gcode filename =
  let file = open_in filename in
  let gcode = 
    try
      `Value (List.of_enum (Gcode.Parser.parse_gcode (Lexing.from_channel file)))
    with exn -> `Exn exn
  in
  close_in file;
  gcode

let gcode_widget ~packing () =
  let filename = load_button_widget ~packing () in
  let gcode_loaded_hook = Hook.create () in
  ignore (Hook.hook filename#chosen (fun filename ->
    match load_gcode filename with
    | `Value gcode -> Hook.issue gcode_loaded_hook gcode
    | `Exn exn -> Printf.printf "Gcode failed to load: %s\n%!" (Printexc.to_string exn)
  ));
  let obj = object
      method gcode_loaded = gcode_loaded_hook
  end in
  obj

let gcode_loader ~packing ~callback ?gcode_filename () =
  let widget = gcode_widget ~packing () in
  ( match gcode_filename with
  | None -> ()
  | Some filename -> 
    match load_gcode filename with
    | `Value v -> callback v
    | `Exn e -> Printf.printf "Cannot load gcode file\n%!"; ()
  );
  ignore (Hook.hook widget#gcode_loaded callback)

let gui sigint_triggered config camera_matrix_arg cnc_camera_offset gcode_filename =
  let video = 
    try new Video.v4l2 
    with exn ->
      Printf.printf "Trouble opening video (%s), not using it\n%!" (Printexc.to_string exn);
      new Video.null
  in
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
  let cnc = 
    try Some (Cnc.connect config.Common.co_device config.Common.co_bps)
    with exn ->
      Printf.printf "Trouble connecting cnc (%s), not using it\n%!" (Printexc.to_string exn);
      None
  in
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
  let mark_cnc_location = ref None in
  let cnc_camera_offset = ref (Option.map v2_of_v3 cnc_camera_offset) in
  let (coord_mode_selection, _) = GEdit.combo_box_text ~strings:["CNC Mode"; "Camera mode"] ~active:(int_of_coord_mode !current_coord_mode) ~packing:control_box#pack () in
  let point_mapping = ref Gg.M3.id in
  let gcode = ref None in
  let env = object
    val gcode_to_cnc = ref None
    method points	   = points
    method point_mapping   = point_mapping
    method liveview	   = liveview
    method camera_to_world = camera_to_world
    method info		   = info
    method video	   = video
    method cnc_control	   = cnc_control
    method gcode	   = gcode
    method gcode_to_cnc    = gcode_to_cnc
  end in
  let set_coord_mode coord_mode =
    ( match !current_coord_mode, coord_mode, !cnc_camera_offset with 
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
    match save_location_difference cnc_camera_offset !mark_cnc_location at with
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
  let _ = 
    match cnc with
    | None -> ()
    | Some cnc -> alignment_widget ~cnc ~packing:control_box#pack env#gcode_to_cnc
  in
  ignore (Hook.hook liveview#overlay (draw_overlay env));
  ignore (Hook.hook cnc_control#position_adjust_callback (cnc_moved env));
  cnc_moved env cnc_control#get_position;
  ( match cnc with
  | None -> ()
  | Some cnc ->
    ignore (Hook.hook liveview#on_button_press (fun cam_xy ->
      Printf.printf "Clicked at %s\n%!" (Gg.V2.to_string cam_xy);
      match !camera_to_world with
      | None -> add_calibration_point env (cam_xy, get_cnc_position cnc);
      | Some camera_to_world-> move_cnc env cam_xy camera_to_world
    ))
  );
  let set_gcode contents = 
    Printf.printf "Loaded %d instructions\n%!" (List.length contents);
    gcode := Some contents
  in
  let _ = gcode_loader ~packing:control_box#pack ~callback:set_gcode ?gcode_filename () in
  ignore (Hook.hook liveview#on_mouse_move (show_location env));
  image_updater env ();
  main_window#show ();
  GMain.Main.main ()

let align = gui
