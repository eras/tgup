open Batteries
open Gtk

let pi2 = 8. *. atan 1.

let verbose = false

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

let (@.) f g x = g (f x)

let v2_of_v3 v3 =
  let open Gg.V3 in
  Gg.V2.v (x v3) (y v3)

let v3_of_v2 v3 =
  let open Gg.V2 in
  Gg.V3.v (x v3) (y v3) 0.0

let v2_of_status_tinyg status = Cnc.(Gg.V2.v status.x status.y)

let flip_y (a, b) = (a, ~-. b)

let angle_of_matrix m3 = Gg.(V2.angle (V2.tr m3 (V2.v 1.0 0.0)))

let scale_of_matrix m3 = 
  let open Gg in
  let vec = V2.v 1.0 1.0 in
  V2.norm (V2.tr m3 vec) /. V2.norm vec

let length_in_matrix m3 len = Gg.(V2.norm (V2.tr m3 (V2.v len 0.0)))

let translation_of_matrix m3 = Gg.(P2.tr m3 (V2.v 0.0 0.0))

let add_calibration_point env (cam_xy, tool_xy) =
  env#points := (cam_xy, tool_xy)::!(env#points);
  ( match !(env#points) with
  | (xy1, tool_xy1)::_::_ ->
    let open Gg in
    let (xy2, tool_xy2) = List.hd (List.rev !(env#points)) in
    let dcam_xy = V2.sub xy2 xy1 in
    let dtool_xy = V2.sub tool_xy1 tool_xy2 in (* consider cnc movement to the opposing direction negative *)
    if verbose then Printf.printf "image point: (%f,%f)\n" (V2.x dcam_xy) (V2.y dcam_xy);
    if verbose then Printf.printf "tool point: (%f,%f)\n" (V2.x dtool_xy) (V2.y dtool_xy);
    let angle = V2.angle dtool_xy -. V2.angle dcam_xy in
    if verbose then Printf.printf "Angle: %f\n%!" (angle /. pi2 *. 360.0);
    let scale' = V2.norm dtool_xy /. V2.norm dcam_xy in
    if verbose then Printf.printf "Scale: %f\n%!" scale';

    let open M3 in
    let m = id in
    let m = mul m (rot angle) in
    let m = mul m (scale2 (V2.v scale' scale')) in
    env#liveview#set_angle (angle_of_matrix m);
    if verbose then Printf.printf "camera_to_world: %s\n%!" (M3.to_string m);
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
	    if verbose then Printf.printf "%d %.2f  \r%!" !frames (float !frames /. (now -. t0));
	    env#liveview#set_image ((640, 480), rgb);
	    wait_io ();
	    false 
	);
      true
    in
    env#video#start ();
    ignore (update_image [])

let tool_moved env xy_delta =
  match !(env#camera_to_world) with
  | None -> ()
  | Some camera_to_world ->
    let open Utils.Matrix in
    let world_to_camera = M3.inv camera_to_world in
    if verbose then Printf.printf "Moved by %f, %f\n%!" (V2.x xy_delta) (V2.y xy_delta);
    let tool_movement = M3.move (V2.neg xy_delta) in
    if verbose then Printf.printf "Matrix: %s\n%!" (M3.to_string tool_movement);
    if verbose then Printf.printf "Translation of 0.0: %s\n%!" (M3.to_string tool_movement);
    let orig = camera_to_world *| !(env#point_mapping) in
    env#point_mapping := world_to_camera *| ((M3.scale2 @@ V2.v 1.0 1.0) *|| tool_movement) *| orig;
    if verbose then Printf.printf "Point mapping: %s\n%!" (M3.to_string !(env#point_mapping))

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

let minmax (x, y) = (min x y, max x y)

let for_float_range x0 x_step x1 f =
  let rec loop x =
    if x < x1 then 
      let _ = () in
      f x;
      loop (x +. x_step)
    else 
      ()
  in
  loop x0

let render_grid cairo world_to_camera (x0, x1, y0, y1) =
  let open Cairo in
  let cairo_to_world = (Gg.M3.inv (Utils.Matrix.m3_of_cairo_matrix @@ Cairo.get_matrix cairo)) in
  select_font_face cairo "Georgia" ~weight:Bold;
  set_font_size cairo (length_in_matrix cairo_to_world 20.0);
  let text_angle = angle_of_matrix world_to_camera in
  let _ =
    let m = Utils.Matrix.m3_of_cairo_matrix (get_font_matrix cairo) in
    let open Utils.Matrix in
    let m' = Gg.M3.scale2 (Gg.V2.v 1.0 ~-.1.0) *| m in
    set_font_matrix cairo (Utils.Matrix.cairo_matrix_of_m3 m')
  in
  let camera_to_world = Gg.M3.inv world_to_camera in
  let map x y = 
    let xy = Gg.V2.v x y in
    let (x', y') = Gg.(V2.to_tuple @@ P2.tr world_to_camera xy) in
    (x', y')
  in
  let map' x y = 
    let xy = Gg.V2.v x y in
    let (x', y') = Gg.(V2.to_tuple @@ P2.tr camera_to_world xy) in
    (x', y')
  in
  (* limits in camera coordinates *)
  let ((x0, y0), (x1, y1)) = (map' x0 y0, map' x1 y1) in
  let ((x0, x1), (y0, y1)) = (minmax (x0, x1), minmax (y0, y1)) in
  let mapped f ~x ~y =
    let (x', y') = map x y in
    f ~x:x' ~y:y'
  in
  set_line_width cairo 1.0;
  set_source_rgba cairo 0.0 0.0 0.0 0.5;
  let density = 1.0 in
  let align f = f -. mod_float f density in
  for_float_range (align (x0 -. density)) density (align (x1 +. density))
    (fun x ->
      save cairo;
      mapped (move_to cairo) ~x ~y:(y0 +. 1.0);
      rotate cairo text_angle;
      Printf.ksprintf (show_text cairo) "% 3.0f" x;
      restore cairo;
      mapped (move_to cairo) ~x ~y:y0;
      mapped (line_to cairo) ~x ~y:y1;
      stroke cairo;
    );
  for_float_range (align (y0 -. density)) density (align (y1 +. density))
    (fun y ->
      save cairo;
      mapped (move_to cairo) ~x:(x0 +. 1.0) ~y;
      rotate cairo text_angle;
      Printf.ksprintf (show_text cairo) "% 3.0f" y;
      restore cairo;
      mapped (move_to cairo) ~x:x0 ~y;
      mapped (line_to cairo) ~x:x1 ~y;
      stroke cairo;
    )

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
    let open Utils.Matrix in
    let tool_mapping = Gg.M3.move (Gg.V2.neg env#cnc_control#get_position) in
    render_grid cairo (tool_mapping *|| world_to_camera) liveview_context#bounds;
    let gcode_to_tool = (Option.default Gg.M3.id !(env#gcode_to_tool)) in
    let matrix = world_to_camera *| tool_mapping *| gcode_to_tool in
    Option.may (render_gcode cairo matrix) !(env#gcode)

let move_tool env cam_xy camera_to_world =
  (* Move the most recently clicked point over the center *)
  let open Gg in
  let move = P2.tr camera_to_world cam_xy in
  env#cnc_control#adjust_position move

let save_location var at = 
  var := Some at

let location_label at =
  let open Gg.V3 in
  Printf.sprintf "X:%.3f Y:%.3f Z:%.3f" (x at) (y at) (z at)

let mark_location_widget ~label ~packing ?tooltip cnc_control f =
  let tooltips = GData.tooltips () in
  let mark_box = GPack.hbox ~packing () in
  let mark_button = GButton.button ~label ~packing:mark_box#pack () in
  ( match tooltip with 
  | None -> ()
  | Some text -> tooltips#set_tip mark_button#coerce ~text);
  let mark_label = GMisc.label ~packing:mark_box#pack () in
  mark_label#set_label "Unset";
  let set_mark event =
    let location = v3_of_v2 cnc_control#get_position in
    mark_label#set_label (f location);
  in
  ignore (mark_button#connect#clicked ~callback:set_mark)

let get_tool_position cnc =
  let status = Cnc.wait cnc Cnc.status_tinyg in
  Gg.V2.v status.x status.y

let coordinate_translation src dst =
  let open Gg in
  let translation = V2.sub src dst in
  let src_to_dst_matrix =
    let open Utils.Matrix in
    let m = Gg.M3.move translation in
    m
  in
  src_to_dst_matrix 

let coordinate_transformation ~scaled (src1, src2) (dst1, dst2) =
  let open Gg in
  let src_delta = V2.sub src2 src1 in
  let dst_delta = V2.sub dst2 dst1 in
  let angle = V2.angle src_delta -. V2.angle dst_delta in
  let scale' = V2.norm src_delta /. V2.norm dst_delta in
  let src_to_dst_matrix =
    let open M3 in
    let m = id in
    let open Utils.Matrix in
    let m = 
      if scaled
      then m *| scale2 (V2.v scale' scale') 
      else m in
    let m = m *| rot angle in
    let m = Gg.M3.move (V2.sub src1 (V2.tr m dst1)) *| m in
    m
  in
  src_to_dst_matrix

let alignment_widget ~cnc_control ~packing gcode_to_tool_var =
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
  let set_matrix gcode_to_tool =
    gcode_to_tool_var := Some gcode_to_tool;
    let text = 
      let open Gg in
      let angle = angle_of_matrix gcode_to_tool in
      let scale' = scale_of_matrix gcode_to_tool in
      let translation = translation_of_matrix gcode_to_tool in
      Printf.sprintf "angle: %.2f scale : %.3f\ntranslation: %s\n%s"
	(angle /. Float.pi *. 180.0)
	scale'
	(V2.to_string translation)
	(M3.to_string gcode_to_tool)
    in
    results#set_text text
  in
  let mark_callback cur_mark v =
    cur_mark := Some (v, cnc_control#get_position);
    match !mark1, !mark2 with
    | Some (gcode1, tool1), None ->
      let gcode_to_tool = coordinate_translation tool1 gcode1 in
      set_matrix gcode_to_tool
    | Some (gcode1, tool1), Some (gcode2, tool2) ->
      let gcode_to_tool = coordinate_transformation ~scaled:false (tool1, tool2) (gcode1, gcode2) in
      let _ =
	if false then
	  let open Gg in
          let m = gcode_to_tool in
          Printf.printf "Point 1 %s translated to tool: %s (should be %s)\n%!" (V2.to_string gcode1) (V2.to_string (P2.tr m gcode1)) (V2.to_string tool1);
          Printf.printf "Point 2 %s translated to tool: %s (should be %s)\n%!" (V2.to_string gcode2) (V2.to_string (P2.tr m gcode2)) (V2.to_string tool2);
          Printf.printf "Delta 1 %s translated to tool: %s (should be %s)\n%!" (V2.to_string (V2.sub gcode2 gcode1)) (V2.to_string (V2.tr m (V2.sub gcode2 gcode1))) (V2.to_string (V2.sub tool2 tool1));
      in
      set_matrix gcode_to_tool
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

(* The need of this function probably is indicative of a bug in LablGTK2? *)
let cast_button_signal_as_widget_signal (x : ([`button], unit -> unit) GtkSignal.t) : (Gtk.widget, unit -> unit) GtkSignal.t = Obj.magic x

let gcode_transform gcode matrix filename =
  let gcode = GcodeMapper.transform matrix (List.enum gcode) in
  File.with_file_out filename @@ fun output ->
    Enum.iter (IO.write_line output) gcode

let gcode_transformer ~packing ~callback () =
  let save_button = GButton.button ~label:"Save" ~packing () in
  save_button#connect#clicked ~callback

let gui sigint_triggered config camera_matrix_arg (tool_camera_offset : Gg.V2.t option) gcode_filename video_device =
  let accel_group = GtkData.AccelGroup.create () in
  let video = 
    try new Video.v4l2 video_device
    with exn ->
      Printf.printf "Trouble opening video (%s), not using it\n%!" (Printexc.to_string exn);
      new Video.null
  in
  let main_window = GWindow.window ~border_width:10 () in
  Gobject.set GtkBaseProps.Window.P.allow_shrink main_window#as_window true;
  ignore (main_window#connect#destroy ~callback:destroy);
  ignore (sigint_triggered#add_persistent_callback (fun () -> destroy ()));
  let vbox = GPack.vbox ~packing:main_window#add () in
  main_window#add_accel_group accel_group;
  let quit_button = GButton.button ~label:"Quit" ~packing:(vbox#pack ~expand:false) () in
  quit_button#misc#add_accelerator ~sgn:(cast_button_signal_as_widget_signal GtkButtonProps.Button.S.activate) ~group:accel_group ~modi:[`CONTROL] GdkKeysyms._q;
  ignore (quit_button#connect#clicked ~callback:destroy);
  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~fill:true ~padding:5) () in
  let hbox = GPack.hbox ~packing:vbox#add () in
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
  | CncControl.CoordModeTool -> 0
  | CncControl.CoordModeCamera -> 1
  in
  let coord_mode_of_int = function
  | 0 -> CncControl.CoordModeTool
  | 1 -> CncControl.CoordModeCamera
  | _ -> assert false
  in
  let points = ref [] in
  let mark_tool_location = ref None in
  let (coord_mode_selection, _) = GEdit.combo_box_text ~strings:["Tool mode"; "Camera mode"] ~active:(int_of_coord_mode cnc_control#get_coord_mode) ~packing:control_box#pack () in
  let point_mapping = ref Gg.M3.id in
  let gcode = ref None in
  let env = object
    val gcode_to_tool = ref None
    method points	   = points
    method point_mapping   = point_mapping
    method liveview	   = liveview
    method camera_to_world = camera_to_world
    method info		   = info
    method video	   = video
    method cnc_control	   = cnc_control
    method gcode	   = gcode
    method gcode_to_tool    = gcode_to_tool
  end in
  let set_coord_mode coord_mode =
    cnc_control#set_coord_mode coord_mode;
    coord_mode_selection#set_active (int_of_coord_mode cnc_control#get_coord_mode)
  in
  ignore (coord_mode_selection#connect#changed (fun () ->
    set_coord_mode (coord_mode_of_int coord_mode_selection#active)
  ));
  let _ = mark_location_widget ~label:"Mark" ~tooltip:"Mark the current position of drill" cnc_control (tap (save_location mark_tool_location) @. location_label) ~packing:control_box#pack in
  let set_camera_offset camera_at =
    match !mark_tool_location with
    | None -> ""
    | Some tool_at ->
      let offset = (Gg.V3.sub camera_at tool_at) in
      cnc_control#set_camera_offset (v2_of_v3 offset);
      location_label offset
  in
  let _ = mark_location_widget ~label:"Camera\noffset" ~tooltip:"Measure distance between camera and drill mark" cnc_control set_camera_offset ~packing:control_box#pack in
  alignment_widget ~cnc_control ~packing:control_box#pack env#gcode_to_tool;
  ignore (Hook.hook liveview#overlay (draw_overlay env));
  ignore (Hook.hook cnc_control#position_adjust_callback (tool_moved env));
  tool_moved env cnc_control#get_position;
  ignore (Hook.hook liveview#on_button_press (fun cam_xy ->
    if verbose then Printf.printf "Clicked at %s\n%!" (Gg.V2.to_string cam_xy);
    match !camera_to_world with
    | None -> add_calibration_point env (cam_xy, cnc_control#get_position);
    | Some camera_to_world-> move_tool env cam_xy camera_to_world
  ));
  let set_gcode contents = 
    Printf.printf "Loaded %d instructions\n%!" (List.length contents);
    gcode := Some contents
  in
  gcode_loader ~packing:control_box#pack ~callback:set_gcode ?gcode_filename ();
  let _ =
    let callback () =
      match !(env#gcode), !(env#gcode_to_tool) with
      | Some gcode, Some gcode_to_tool ->
	gcode_transform gcode gcode_to_tool "transformed.gcode"
      | _ -> ()
    in
    gcode_transformer ~packing:control_box#pack ~callback ();
  in
  ignore (Hook.hook liveview#on_mouse_move (show_location env));
  image_updater env ();
  main_window#show ();
  GMain.Main.main ()

let align = gui
