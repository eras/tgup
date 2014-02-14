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

let gui config camera_matrix_arg =
  let video = V4l2.init "/dev/video0" { width = 640; height = 480 } in

  let main_window = GWindow.window ~border_width:10 () in
  Gobject.set GtkBaseProps.Window.P.allow_shrink main_window#as_window true;
  ignore (main_window#connect#destroy ~callback:destroy);
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
  let info = GMisc.label ~packing:(control_box#pack) () in
  let io_watch = ref None in
  let t0 = Unix.gettimeofday () in
  let frames = ref 0 in
  let points = ref [] in
  let point_mapping = ref Gg.M3.id in
  liveview#overlay := (fun cairo ->
    let open Cairo in
    set_source_rgba cairo 0.0 1.0 0.0 0.5;
    arc cairo 0.0 0.0 20.0 0.0 pi2;
    fill cairo;
    set_source_rgba cairo 1.0 0.0 0.0 0.5;
    flip List.iter !points @@ fun ((xy), _) ->
      let (x, y) = Gg.(V2.to_tuple @@ P2.tr !point_mapping xy) in
      (* Printf.printf "Resulting point: %f, %f\n%!" x y; *)
      arc cairo x y 10.0 0.0 pi2;
      fill cairo
  );
  cnc_control#position_adjust_callback := (fun (x_ofs, y_ofs) ->
    match !camera_to_world with
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
      let orig = camera_to_world * !point_mapping in
      point_mapping := world_to_camera * ((M3.scale2 @@ V2.v 1.0 1.0) *| cnc_movement) * orig;

      Printf.printf "Point mapping: %s\n%!" (M3.to_string !point_mapping);
  );
  liveview#on_button_press := (fun xy ->
    let cam_xy = Gg.V2.of_tuple xy in
    Printf.printf "Clicked at %s\n%!" (Gg.V2.to_string cam_xy);
    match !camera_to_world with
    | None -> 
      points := (cam_xy, Gg.V2.of_tuple cnc_control#get_position)::!points;
      ( match !points with
      | (xy1, cnc_xy1)::_::_ ->
	let open Gg in
	let (xy2, cnc_xy2) = List.hd (List.rev !points) in
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
	liveview#set_angle (angle_of_matrix m);
	Printf.printf "camera_to_world: %s\n%!" (M3.to_string m);
	camera_to_world := Some m;
      | _ -> ()
      )
    | Some camera_to_world->
      (* Move the most recently clicked point over the center *)
      let open Gg in
      let move = P2.tr camera_to_world cam_xy in
      cnc_control#adjust_position (V2.x move) (V2.y move);
    | _ -> ()
  );
  liveview#on_mouse_move := (fun xy ->
    let cam_xy = Gg.V2.of_tuple xy in
    match !camera_to_world with
    | None -> ()
    | Some camera_to_world ->
      let open Gg in
      let world_to_camera = M3.inv camera_to_world in
      let cam_in_world = V2.tr camera_to_world cam_xy in
      info#set_label (Printf.sprintf "%s\n%s" (V2.to_string cam_xy) (V2.to_string cam_in_world));
      ()
  );
  let rec wait_io () = 
    io_watch := Some (
      GMain.Io.add_watch
        ~cond:[`IN]
        ~callback:update_image
        (GMain.Io.channel_of_descr (V4l2.get_fd video))
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
	  let frame = V4l2.get_frame video in
	  let rgb = frame#rgb_ba in
	  let now = Unix.gettimeofday () in
	  incr frames;
	  Printf.printf "%d %.2f  \r%!" !frames (float !frames /. (now -. t0));
	  liveview#set_image ((640, 480), rgb);
	  wait_io ();
	  false 
      );
    true
  in
  V4l2.start video;
  ignore (update_image []);
  main_window#show ();
  GMain.Main.main ()

let align sigint_triggered config = 
  gui config
