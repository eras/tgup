open Batteries
open Gtk

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

let gui () =
  let video = V4l2.init "/dev/video0" { width = 640; height = 480 } in

  let main_window = GWindow.window ~border_width:10 () in
  Gobject.set GtkBaseProps.Window.P.allow_shrink main_window#as_window true;
  ignore (main_window#connect#destroy ~callback:destroy);
  let vbox = GPack.vbox ~packing:main_window#add () in
  let quit_button = GButton.button ~label:"Quit" ~packing:(vbox#pack ~expand:false) () in
  ignore (quit_button#connect#clicked ~callback:destroy);
  let liveview = LiveView.view ~packing:vbox#add (640, 480) () in
  let io_watch = ref None in
  let t0 = Unix.gettimeofday () in
  let frames = ref 0 in
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
	  let rgb = ba_of_string frame#rgb in
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
  gui ()
