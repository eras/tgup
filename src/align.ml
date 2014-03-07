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

let v3_of_status_tinyg status = Cnc.(Gg.V3.v status.x status.y status.z)

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

let tool_moved env xyz_delta =
  match !(env#camera_to_world) with
  | None -> ()
  | Some camera_to_world ->
    let open Utils.Matrix in
    let world_to_camera = M3.inv camera_to_world in
    if verbose then Printf.printf "Moved by %s\n%!" (V3.to_string xyz_delta);
    let tool_movement = M3.move (V2.neg @@ v2_of_v3 xyz_delta) in
    if verbose then Printf.printf "Matrix: %s\n%!" (M3.to_string tool_movement);
    if verbose then Printf.printf "Translation of 0.0: %s\n%!" (M3.to_string tool_movement);
    let orig = camera_to_world *| !(env#point_mapping) in
    env#point_mapping := world_to_camera *| ((M3.scale2 @@ V2.v 1.0 1.0) *|| tool_movement) *| orig;
    if verbose then Printf.printf "Point mapping: %s\n%!" (M3.to_string !(env#point_mapping))

let render_gcode cairo mapping_matrix (gcode : Gcode.Evaluate.step_result list) =
  let open Cairo in
  let open Gcode.Evaluate in
  set_line_width cairo (length_in_matrix mapping_matrix 3.0);
  set_line_cap cairo ROUND;
  set_line_join cairo JOIN_ROUND;
  let raw_coords state x_key y_key =
    let x = AxisMap.find x_key state.ms_position in
    let y = AxisMap.find y_key state.ms_position in
    Gg.V2.v x y
  in
  let coords state x_key y_key =
    let xy = raw_coords state x_key y_key in
    Gg.P2.tr mapping_matrix xy
  in
  let raw_regs state x_key y_key =
    let x = RegNoAxisMap.find x_key state.ms_regs in
    let y = RegNoAxisMap.find y_key state.ms_regs in
    Gg.V2.v x y
  in
  let draw =
    let drawn = Hashtbl.create 10240 in
    fun key f ->
      if Hashtbl.mem drawn key
      then ()
      else (
	let draw = f () in
	Hashtbl.add drawn key draw;
	draw ()
      )
  in  
  let rec loop = function
    | [] -> ()
    | step_result::rest ->
      let _ =
	let open Gcode.Evaluate in
	match motion_of_commands step_result.sr_commands with
	| Some (`G1) ->
	  let xy0 = raw_coords step_result.sr_state0 `X `Y in
	  let xy1 = raw_coords step_result.sr_state1 `X `Y in
	  draw (`G1 (xy0, xy1)) (fun () ->
	    let (x0, y0) = Gg.V2.to_tuple @@ Gg.P2.tr mapping_matrix xy0 in
	    let (x1, y1) = Gg.V2.to_tuple @@ Gg.P2.tr mapping_matrix xy1 in
	    fun () ->
	      move_to cairo x0 y0;
	      line_to cairo x1 y1;
	      stroke cairo
	  )
	| Some (`G2) ->
	  let open Gg in
	  let raw_xy0 = raw_coords step_result.sr_state0 `X `Y in
	  let raw_ij = raw_regs step_result.sr_state1 `I `J in
	  let raw_xy1 = raw_coords step_result.sr_state1 `X `Y in
	  draw (`G2 (raw_xy0, raw_ij, raw_xy1)) (fun () ->
	    let ij = Gg.V2.tr mapping_matrix raw_ij in
	    let xy0 = Gg.P2.tr mapping_matrix raw_xy0 in
	    let xy1 = Gg.P2.tr mapping_matrix raw_xy1 in
	    let center = V2.add xy0 ij in
	    let ang0 = V2.angle (V2.sub xy0 center) in
	    let ang1 = V2.angle (V2.sub xy1 center) in
	    let ang1 = if ang1 < ang0 then ang1 +. Gg.Float.two_pi else ang1 in
	    let radius = Gg.V2.norm ij in
	    fun () ->
	      arc cairo (V2.x center) (V2.y center) radius ang1 ang0;
	      stroke cairo
	  )
	| _ -> ()
      in
      loop rest
  in
  set_source_rgba cairo 1.0 0.0 0.0 1.0;
  loop gcode

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

(* Round the value up to the next 10^n *)
let nice_grid_density f =
  let l = log10 f in
  let (fractional, integral) = modf l in
  let integral =
    if l > 0.0 && fractional <> 0.0
    then integral +. 1.0
    else integral
  in
  10.0 ** integral

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
  let density = nice_grid_density (length_in_matrix cairo_to_world 0.2) in
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
  let post_render =
  ( match !(env#camera_to_world) with
  | None -> ignore
  | Some camera_to_world ->
    let world_to_camera = Gg.M3.inv camera_to_world in
    let open Utils.Matrix in
    let tool_mapping = Gg.M3.move (Gg.V2.neg env#cnc_control#get_tool_position) in
    let matrix = world_to_camera *| tool_mapping *| !(env#gcode_to_tool) in
    if !(env#show_gcode) then Option.may (render_gcode cairo matrix) !(env#gcode);
    fun () -> render_grid cairo (tool_mapping *|| world_to_camera) liveview_context#bounds
  ) in
  ( set_source_rgba cairo 0.0 1.0 0.0 0.5;
    arc cairo 0.0 0.0 20.0 0.0 pi2;
    fill cairo;
    flip List.iter !(env#points) @@ fun ((xy), _) ->
      let (x, y) = Gg.(V2.to_tuple @@ P2.tr !(env#point_mapping) xy) in
    (* Printf.printf "Resulting point: %f, %f\n%!" x y; *)
      arc cairo x y 10.0 0.0 pi2;
      fill cairo );
  post_render ()

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
    let location = cnc_control#get_position in
    mark_label#set_label (f location);
  in
  ignore (mark_button#connect#clicked ~callback:set_mark)

let get_tool_position cnc =
  let Cnc.ResultOK status = Cnc.wait cnc Cnc.status_tinyg in
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
    gcode_to_tool_var := gcode_to_tool;
    let text = 
      let open Gg in
      let angle = angle_of_matrix gcode_to_tool in
      let scale' = scale_of_matrix gcode_to_tool in
      let translation = translation_of_matrix gcode_to_tool in
      Printf.sprintf "angle: %.3f scale : %.5f\ntranslation: %s"
	(angle /. Float.pi *. 180.0)
	scale'
	(V2.to_string translation)
    in
    results#set_text text
  in
  let scaled = ref false in
  let _ = Widgets.toggle_widget ~packing:vbox#pack ~var:scaled "Scaled" () in
  let mark_callback cur_mark reference =
    cur_mark := Some (reference, cnc_control#get_tool_position);
    match !mark1, !mark2 with
    | Some (gcode1, tool1), None ->
      let gcode_to_tool = coordinate_translation tool1 gcode1 in
      set_matrix gcode_to_tool
    | Some (gcode1, tool1), Some (gcode2, tool2) ->
      let gcode_to_tool = coordinate_transformation ~scaled:!scaled (tool1, tool2) (gcode1, gcode2) in
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
      `Value (List.of_enum (Gcode.Evaluate.evaluate_gcode (Lexing.from_channel file)))
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
    | `Exn e -> Printf.printf "Cannot load gcode file: %s\n%!" (Printexc.to_string e); ()
  );
  ignore (Hook.hook widget#gcode_loaded callback)

(* The need of this function probably is indicative of a bug in LablGTK2? *)
let cast_button_signal_as_widget_signal (x : ([`button], unit -> unit) GtkSignal.t) : (Gtk.widget, unit -> unit) GtkSignal.t = Obj.magic x

let gcode_transform gcode matrix z_offset filename =
  let gcode = GcodeMapper.transform matrix z_offset (List.enum gcode) in
  let strings = Enum.map Gcode.Evaluate.string_of_step_result gcode in
  File.with_file_out filename @@ fun output ->
    Enum.iter (IO.write_line output) strings

let gcode_transformer ~packing ~callback () =
  let save_button = GButton.button ~label:"Save" ~packing () in
  save_button#connect#clicked ~callback

let upload_widget ~packing () =
  let run_button = GButton.button ~label:"Run CNC job" ~packing () in
  let abort_button = GButton.button ~label:"ABORT" ~packing () in
  let abort_info = GMisc.label ~packing () in
  let progress = GRange.progress_bar ~packing ~show:false () in
  abort_button#misc#set_sensitive false;
  let o = object
      method run_button_connect callback = run_button#connect#clicked ~callback
      method abort_button_connect callback = abort_button#connect#clicked ~callback
      method set_abort_text text = abort_info#set_text text
      method set_progress fraction text =
        progress#set_fraction fraction;
        progress#set_text text
      method set_running running = 
        let enable_progress = running in
	let enable_run = not running in
	let enable_abort = running in 
        if enable_abort then abort_info#set_text "";
        if enable_progress then progress#misc#show() else progress#misc#hide ();
	run_button#misc#set_sensitive enable_run;
	abort_button#misc#set_sensitive enable_abort;
  end in
  o

let start_upload_program program cnc =
  let strings =
    flip Enum.map program @@ fun (sr : Gcode.Evaluate.step_result) ->
      (sr.sr_state1.ms_orig_line_number, Gcode.Evaluate.string_of_step_result sr) in
  let line_sent = Hook.create () in
  let abort = ref false in
  let o = 
    let finished = new Future.t in
    ( object
      method finished = finished
      method line_sent = line_sent
      method abort = abort := true
      end )
  in
  let rec loop () =
    if !abort
    then o#finished#set `Failure
    else 
      match Enum.get strings with
      | None -> o#finished#set `Success
      | Some (orig_line_nuber, "") ->
        Hook.issue line_sent orig_line_nuber;
        loop ()
      | Some (orig_line_nuber, str) ->
        Cnc.async cnc (Cnc.raw_gcode str) @@ function 
	| ResultOK () ->
          Hook.issue line_sent orig_line_nuber;
          loop ()
	| ResultDequeued -> o#finished#set `Failure
  in
  Cnc.async cnc Cnc.set_absolute (function
  | ResultOK () -> loop ();
  | ResultDequeued -> o#finished#set `Failure 
  );
  o

let setup_upload upload_widget env =
  let last_line_with_positive_z = ref None in
  let aborting = ref false in
  let run_callback () =
    match !(env#gcode), env#cnc with
    | Some gcode, Some cnc ->
      upload_widget#set_running true;
      env#cnc_control#set_enabled false;
      let gcode' = List.of_enum @@ GcodeMapper.transform !(env#gcode_to_tool) (Option.default 0.0 env#cnc_control#get_tool_level) (List.enum gcode) in
      let last_line =
        let num_lines = List.length gcode' in
        if num_lines = 0
        then 1
        else (List.nth gcode' (num_lines - 1)).sr_state1.ms_orig_line_number
      in
      let input_by_line = 
        let h = Hashtbl.create last_line in
        flip List.iter gcode' (fun sr ->
          Hashtbl.add h sr.sr_state1.ms_orig_line_number sr
        );
        fun n -> Hashtbl.find h n
      in
      let upload = start_upload_program (List.enum gcode') cnc in
      let t0 = Unix.gettimeofday () in
      let status_report_hook = Hook.hook (Cnc.status_report_tinyg cnc) (
        fun status ->
          let now = Unix.gettimeofday () in
          let progress = float status.line /. float last_line in
          let time_left = (now -. t0) /. progress in
          let time_finished = t0 +. time_left in
          let z = Gcode.Evaluate.AxisMap.find `Z (input_by_line status.line).sr_state1.ms_position in
          if z > 0.0 then last_line_with_positive_z := Some status.line;
          env#cnc_control#set_position (v3_of_status_tinyg status);
          upload_widget#set_progress
            progress
            (Printf.sprintf "Line %d/%d, ETA %s at %s" status.line last_line
               (Utils.human_eta (int_of_float time_left))
               (try Utils.string_of_time time_finished with Unix.Unix_error (Unix.EINVAL, _, _) -> "-"));
      ) in
      upload#finished#add_persistent_callback (
	function `Failure | `Success ->
	  Cnc.async cnc (Cnc.wait_status_tinyg @@ fun status ->
	    match status.stat with
	    | 3 -> Some ()
	    | _ -> None
	  ) (function _ ->
	    Printf.printf "Program upload complete!\n%!";
            Hook.unhook status_report_hook;
	    upload_widget#set_running false;
	    env#cnc_control#set_enabled true;
            aborting := false
	  )
      );
      ()
    | _ -> ()
  in
  let abort_callback () =
    Printf.printf "Try abort..\n";
    if not !aborting then (
      Printf.printf "Do abort!\n";
      aborting := true;
      upload_widget#set_abort_text (
        Printf.sprintf "Last line with positive Z: %s"
          (Option.default "-" (Option.map string_of_int !last_line_with_positive_z))
      );
      Cnc.async (Option.get env#cnc) Cnc.feed_hold (fun _ ->
        (* TODO: better way to do this? wait for state to be in feedhold and then resume? *)
        ignore @@ Glib.Timeout.add ~ms:1000 ~callback:(fun () ->
          Cnc.ignore (Option.get env#cnc) Cnc.feed_resume;
          false
        )
      )
    )
  in
  upload_widget#run_button_connect run_callback;
  upload_widget#abort_button_connect abort_callback

let gui sigint_triggered config camera_matrix_arg (tool_camera_offset : Gg.V2.t option) camera_z tool_z gcode_filename video_device =
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
  let cnc_control = CncControl.view ~camera_z ~tool_z ~packing:(control_box#pack) cnc () in
  Option.may cnc_control#set_camera_offset tool_camera_offset;
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
  let show_gcode = ref true in
  let env = object
    val gcode_to_tool = ref Gg.M3.id
    method points	   = points
    method point_mapping   = point_mapping
    method liveview	   = liveview
    method camera_to_world = camera_to_world
    method info		   = info
    method video	   = video
    method cnc_control	   = cnc_control
    method cnc             = cnc
    method gcode	   = gcode
    method gcode_to_tool   = gcode_to_tool
    method show_gcode      = show_gcode
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
    | None -> add_calibration_point env (cam_xy, v2_of_v3 cnc_control#get_position);
    | Some camera_to_world-> move_tool env cam_xy camera_to_world
  ));
  let set_gcode contents = 
    Printf.printf "Loaded %d instructions\n%!" (List.length contents);
    gcode := Some contents
  in
  gcode_loader ~packing:control_box#pack ~callback:set_gcode ?gcode_filename ();
  Widgets.toggle_widget ~packing:control_box#pack ~var:env#show_gcode "Show G-code" ();
  let _ =
    let callback () =
      match !(env#gcode) with
      | Some gcode ->
	gcode_transform gcode !(env#gcode_to_tool) (Option.default 0.0 cnc_control#get_tool_level) "transformed.gcode"
      | _ -> ()
    in
    gcode_transformer ~packing:control_box#pack ~callback ();
  in
  let _ =
    let upload_widget = upload_widget ~packing:control_box#pack () in
    setup_upload upload_widget env
  in
  ignore (Hook.hook liveview#on_mouse_move (show_location env));
  image_updater env ();
  main_window#show ();
  GMain.Main.main ()

let align = gui
