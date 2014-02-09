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
  let update_image conditions =
    let frame = V4l2.get_frame video in
    let rgb = ba_of_string frame#rgb in
    liveview#set_image ((640, 480), rgb);
    true
  in
  V4l2.start video;
  update_image [];
  GMain.Io.add_watch
    ~cond:[`IN]
    ~callback:update_image
    (GMain.Io.channel_of_descr (V4l2.get_fd video));
  main_window#show ();
  GMain.Main.main ()

let align sigint_triggered config = 
  gui ()
