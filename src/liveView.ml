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

let cairo_matrix_of_m3 m = 
  let open Cairo in
  let open Gg.M3 in
  {
    xx = e00 m;
    xy = e01 m;
    x0 = e02 m;
    yx = e10 m;
    yy = e11 m;
    y0 = e12 m;
  }

let view (width, height) ?(angle=0.0) ?packing () =
  let drawing_area = GMisc.drawing_area ?packing ~width ~height () in
  let image = ref None in
  let inverse_transformation_matrix = ref None in
  let overlay = Hook.create () in
  let angle = ref angle in
  let draw cr area_width area_height =
    let open Cairo in
    let r = 0.25 *. area_width in
    set_source_rgba cr 0. 1. 0. 0.5;
    match !image with
    | None -> 
      arc cr (0.5 *. area_width) (0.35 *. area_height) r 0. pi2;
      fill cr;
    (* set_source_rgba cr 1. 0. 0. 0.5; *)
      arc cr (0.35 *. area_width) (0.65 *. area_height) r 0. pi2;
      fill cr;
    (* set_source_rgba cr 0. 0. 1. 0.5; *)
      arc cr (0.65 *. area_width) (0.65 *. area_height) r 0. pi2;
      fill cr
    | Some (image, image_width, image_height) ->
      let (im_width, im_height) = (float image_width, float image_height) in
      let aspect = im_width /. im_height in
      let x_scale, y_scale =
	if area_width /. area_height > aspect 
	then (area_height /. im_height, area_height /. im_height)
	else (area_width /. im_width, area_width /. im_width)
      in

      let open Gg in
      let ( *| ) = M3.mul in
      let ( *|| ) a b = M3.mul b a in
      let m = M3.id in
      let m = M3.move (V2.v (im_height /. 2.0) (im_width /. 2.0))	  *|| m in
      let m = M3.rot (-. !angle)					  *|| m in
      let m = M3.move (V2.v (~-. im_width /. 2.0) (~-. im_height /. 2.0)) *|| m in
      let m = M3.scale2 (V2.v x_scale y_scale)				  *|| m in
      let m = M3.move (V2.v (area_width /. 2.0) (area_height /. 2.0))	  *|| m in

      set_matrix cr (cairo_matrix_of_m3 m);

      set_source_surface cr image ~x:(~-.im_width /. 2.0) ~y:(~-.im_height /. 2.0);
      rectangle cr (~-.im_width /. 2.0) (im_height /. 2.0) (im_width -. 1.0) (~-.im_height -. 1.0);
      fill cr;

      let m_overlay = M3.scale2 (V2.v 1.0 ~-.1.0) *|| m in

      inverse_transformation_matrix := Some (Gg.M3.inv m_overlay);

      set_matrix cr (cairo_matrix_of_m3 m_overlay);
      Hook.issue overlay cr
  in
  let expose ev =
    show_exn @@ fun () ->
      let open Cairo in
      let cr = Cairo_gtk.create drawing_area#misc#window in
      let allocation = drawing_area#misc#allocation in
      draw cr (float allocation.Gtk.width) (float allocation.Gtk.height);
      false
  in
  let on_button_press = Hook.create () in
  let on_mouse_move = Hook.create () in
  let interface = object
    method set_image ((width, height), rgb_data) =
      image := Some (image_of_rgb (width, height) rgb_data, width, height);
      drawing_area#misc#draw None
    method on_button_press = on_button_press
    method on_mouse_move = on_mouse_move
    method overlay = overlay
    method set_angle a = angle := a
  end in
  let button_pressed ev =
    ( match !inverse_transformation_matrix with
    | None -> ()
    | Some matrix ->
      let (x, y) = (GdkEvent.Button.x ev, GdkEvent.Button.y ev) in
      let (x', y') = Gg.V2.to_tuple (Gg.P2.tr matrix (Gg.V2.v x y)) in
      Printf.printf "Pressed at %f, %f\n%!" x' y';
      ignore (Hook.issue on_button_press (x', y'));
      ()
    );
    true
  in
  let mouse_moved ev =
    ( match !inverse_transformation_matrix with
    | None -> ()
    | Some matrix ->
      let (x, y) = (GdkEvent.Motion.x ev, GdkEvent.Motion.y ev) in
      let (x', y') = Gg.V2.to_tuple (Gg.P2.tr matrix (Gg.V2.v x y)) in
      ignore (Hook.issue on_mouse_move (x', y'));
      ()
    );
    true
  in
  ignore (drawing_area#event#connect#expose expose);
  drawing_area#event#add [`EXPOSURE];
  ignore (drawing_area#event#connect#button_press button_pressed);
  ignore (drawing_area#event#connect#motion_notify mouse_moved);
  drawing_area#event#add [`BUTTON_PRESS];
  drawing_area#event#add [`POINTER_MOTION];
  drawing_area#misc#draw None;
  interface
