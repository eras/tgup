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

let p2_to_tuple p = Gg.P2.(x p, y p)

let gui config =
  let video = V4l2.init "/dev/video0" { width = 640; height = 480 } in

  let main_window = GWindow.window ~border_width:10 () in
  Gobject.set GtkBaseProps.Window.P.allow_shrink main_window#as_window true;
  ignore (main_window#connect#destroy ~callback:destroy);
  let vbox = GPack.vbox ~packing:main_window#add () in
  let quit_button = GButton.button ~label:"Quit" ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~fill:true ~padding:5) () in
  let hbox = GPack.hbox ~packing:vbox#add () in
  ignore (quit_button#connect#clicked ~callback:destroy);
  let liveview = LiveView.view ~packing:hbox#add (* ~angle:(15.0 *. ~-. pi2 /. 64.0) *) (640, 480) () in
  let cnc = Cnc.connect config.Common.co_device config.Common.co_bps in
  let cnc_control = CncControl.view ~packing:(hbox#pack ~expand:false ~padding:5) cnc () in
  let io_watch = ref None in
  let t0 = Unix.gettimeofday () in
  let frames = ref 0 in
  let points = ref [] in
  let camera_to_world = ref None in
  let point_mapping = ref Gg.M3.id in
  liveview#overlay := (fun cairo ->
    let open Cairo in
    set_source_rgba cairo 1.0 0.0 0.0 0.5;
    flip List.iter !points @@ fun (((x : float), (y : float)), _) ->
      let (x, y) = Gg.(p2_to_tuple @@ P2.tr !point_mapping (P2.v x y)) in
      Printf.printf "Resulting point: %f, %f\n%!" x y;
      arc cairo x y 10.0 0.0 pi2;
      fill cairo
  );
  cnc_control#position_adjust_callback := (fun (x_ofs, y_ofs) ->
    match !camera_to_world with
    | None -> ()
    | Some camera_to_world ->
      let open Gg in
      let world_to_camera = M3.inv camera_to_world in
      Printf.printf "Moved by %f, %f\n%!" x_ofs y_ofs;
      let cnc_movement = M3.move (V2.v x_ofs (~-.y_ofs)) in
      Printf.printf "Matrix: %s\n%!" (M3.to_string cnc_movement);
      Printf.printf "Translation of 0.0: %s\n%!" (M3.to_string cnc_movement);
      let ( * ) = M3.mul in
      let ( *| ) a b = M3.mul b a in
      let orig = camera_to_world * !point_mapping in
      point_mapping := world_to_camera * ((M3.scale2 @@ V2.v 1.0 1.0) *| cnc_movement) * orig;

      Printf.printf "Point mapping: %s\n%!" (M3.to_string !point_mapping);
  );
  liveview#on_button_press := (fun xy ->
    points := (xy, cnc_control#get_position)::!points;
    match !camera_to_world, !points with
    | None, (xy1, cnc_xy1)::_::_ ->
      let (xy2, cnc_xy2) = List.hd (List.rev !points) in
      let open Vector in
      let dxy = flip_y @@ sub_vector xy2 xy1 in
      let dcnc_xy = sub_vector cnc_xy2 cnc_xy1 in
      Printf.printf "image point: (%f,%f)\n" (fst dxy) (snd dxy);
      Printf.printf "cnc point: (%f,%f)\n" (fst dcnc_xy) (snd dcnc_xy);
      let angle = (acos (dot2 dxy dcnc_xy /. length dxy /. length dcnc_xy)) in
      Printf.printf "Angle: %f\n%!" (angle /. pi2 *. 360.0);
      let scale' = length dcnc_xy /. length dxy in
      Printf.printf "Scale: %f\n%!" scale';

      let open Gg in
      let open M3 in
      let m = id in
      let m = mul m (rot angle) in
      let m = mul m (scale2 (V2.v scale' scale')) in
      liveview#set_angle (angle +. pi);
      camera_to_world := Some m;
    | Some camera_to_world, (xy1, _)::_::_ ->
      (* Move the most recently clicked point over the first clicked point *)
      let (xy2, _) = List.hd (List.rev !points) in
      let dxy = flip_y @@ Vector.sub_vector xy2 xy1 in
      let open Gg.P2 in
      let absolute = tr camera_to_world (v (fst dxy) (snd dxy)) in
      (* Cnc.wait cnc (Cnc.set_feed_rate 100.0); *)
      (* Cnc.wait cnc Cnc.set_absolute; *)
      (* Cnc.wait cnc (Cnc.travel [`X ~-.(x absolute); `Y ~-.(y absolute)]); *)
      Printf.printf "World: %f, %f\n%!" (x absolute) (y absolute);
      let (cnc_x, cnc_y) = cnc_control#get_position in
      let (delta_x, delta_y) = (cnc_x -. (x absolute), cnc_y -. (y absolute)) in
      cnc_control#adjust_position delta_x delta_y;
    | _ -> ()
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
