open Batteries

let (@@) a b = a b

let pi2 = 8. *. atan 1.

let show_exn f =
  try 
    f ()
  with exn ->
    Printf.printf "Exception: %s (%s)\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ());
    raise exn

let image_of_rgb (width, height) rgb_data =
  let open Bigarray in
  let open Array1 in
  let image = Cairo.Image.create Cairo.Image.RGB24  ~width ~height in
  let stride = Cairo.Image.get_stride image in
  let rgbx_data = Cairo.Image.get_data8 image in
  for y = 0 to height - 1 do
    let wr = ref (y * stride * 4) in
    let rd = ref (y * width * 3) in
    for x = 0 to width - 1 do
      for c = 0 to 2 do
      	unsafe_set rgbx_data !wr (unsafe_get rgb_data !rd);
      	incr wr;
      	incr rd;
      done;
      incr wr;
      (* blit (sub rgb_data !rd 3) (sub rgbx_data !wr 3); *)
      (* wr := !wr + 4; *)
      (* rd := !rd + 3; *)
    done
  done;
  image

let view (width, height) ?(angle=0.0) ?packing () =
  let drawing_area = GMisc.drawing_area ?packing ~width ~height () in
  let image = ref None in
  let inverse_transformation_matrix = ref None in
  let overlay = ref (fun cairo -> ()) in
  let draw cr width height =
    let open Cairo in
    let r = 0.25 *. width in
    set_source_rgba cr 0. 1. 0. 0.5;
    match !image with
    | None -> 
      arc cr (0.5 *. width) (0.35 *. height) r 0. pi2;
      fill cr;
    (* set_source_rgba cr 1. 0. 0. 0.5; *)
      arc cr (0.35 *. width) (0.65 *. height) r 0. pi2;
      fill cr;
    (* set_source_rgba cr 0. 0. 1. 0.5; *)
      arc cr (0.65 *. width) (0.65 *. height) r 0. pi2;
      fill cr
    | Some (image, image_width, image_height) ->
      let (im_width, im_height) = (float image_width, float image_height) in
      let aspect = im_width /. im_height in
      let x_scale, y_scale =
	if width /. height > aspect 
	then (height /. im_height, height /. im_height)
	else (width /. im_width, width /. im_width)
      in
      let matrix = Matrix.init_identity () in
      Matrix.translate matrix ~y:(im_width /. 2.0) ~x:(im_height /. 2.0);
      Matrix.rotate matrix angle;
      Matrix.translate matrix ~x:(~-. im_width /. 2.0) ~y:(~-. im_height /. 2.0);
      Matrix.scale matrix x_scale y_scale;

      set_matrix cr matrix;

      Matrix.invert matrix;
      inverse_transformation_matrix := Some matrix;

      set_source_surface cr image ~x:0.0 ~y:0.0;
      rectangle cr 0.0 0.0 im_width im_height;
      fill cr;
      !overlay cr
  in
  let expose ev =
    show_exn @@ fun () ->
      let open Cairo in
      let cr = Cairo_gtk.create drawing_area#misc#window in
      let allocation = drawing_area#misc#allocation in
      draw cr (float allocation.Gtk.width) (float allocation.Gtk.height);
      false
  in
  let on_button_press = ref (fun xy -> ()) in
  let interface = object
    method set_image ((width, height), rgb_data) =
      image := Some (image_of_rgb (width, height) rgb_data, width, height);
      drawing_area#misc#draw None
    method on_button_press = on_button_press
    method overlay = overlay
  end in
  let button_pressed ev =
    ( match !inverse_transformation_matrix with
    | None -> ()
    | Some matrix ->
      let (x, y) = (GdkEvent.Button.x ev, GdkEvent.Button.y ev) in
      let (x', y') = Cairo.Matrix.transform_point matrix ~x ~y in
      Printf.printf "Pressed at %f, %f\n%!" x' y';
      !on_button_press (x', y');
      ()
    );
    true
  in
  ignore (drawing_area#event#connect#expose expose);
  drawing_area#event#add [`EXPOSURE];
  ignore (drawing_area#event#connect#button_press button_pressed);
  drawing_area#event#add [`BUTTON_PRESS];
  drawing_area#misc#draw None;
  interface
