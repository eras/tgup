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

let bounds m (x0, y0) (x1, y1) =
  let open Gg in
  let points =
    List.map
      (fun (x, y) ->
	P2.tr m (V2.v x y)
      )
      [(x0, y0); (x1, y0); (x0, y1); (x1, y1)]
  in
  let extreme choose op =
    List.fold_left (
      fun minima value ->
	if op minima (choose value)
	then minima
	else (choose value)
    ) 
      (choose (List.hd points)) (List.tl points) 
  in
  let left   = extreme V2.x ( < ) in
  let right  = extreme V2.x ( > ) in
  let top    = extreme V2.y ( > ) in
  let bottom = extreme V2.y ( < ) in
  (left, right, top, bottom)

let bounds_image m (im_width, im_height) =
  bounds m
    (~-.im_width /. 2.0, ~-.im_height /. 2.0)
    (im_width /. 2.0, im_height /. 2.0)

let fit m (area_width, area_height) (im_width, im_height) =
  let open Utils.Matrix in
  let (left, right, top, bottom) = bounds_image m (im_width, im_height) in
  let width' = right -. left in
  let height' = top -. bottom in
  let m = M3.move (V2.v (~-.left) top) *| m in
  let x_scale, y_scale =
    if area_width /. area_height > width' /. height' 
    then (area_height /. height', area_height /. height')
    else (area_width /. width', area_width /. width')
  in
  let m = M3.scale2 (V2.v (x_scale) (y_scale)) *| m in
  m

let center m (area_width, area_height) (im_width, im_height) =
  let open Utils.Matrix in
  let (left, right, top, bottom) = bounds_image m (im_width, im_height) in
  let area_center = V2.v (area_width /. 2.0) (area_height /. 2.0) in
  let matrix_center = V2.v ((left +. right) /. 2.0) ((top +. bottom) /. 2.0) in
  let m = M3.move (V2.sub area_center matrix_center) *| m in
  m

let view (width, height) ?(angle=0.0) ?packing () =
  let drawing_area = GMisc.drawing_area ?packing ~width ~height () in
  let image = ref None in
  let inverse_transformation_matrix = ref None in
  let overlay = Hook.create () in
  let angle = ref angle in
  let scale' = ref 1.0 in
  let image_position = ref (Gg.V2.v 0.0 0.0) in
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
      let open Utils.Matrix in
      let (im_width, im_height) = (float image_width, float image_height) in
      let m = M3.id in
      let m = M3.rot (-. !angle) *|| m in
      let m = fit m (area_width, area_height) (im_width, im_height) in
      let m = center m (area_width, area_height) (im_width, im_height) in
      let m = M3.scale2 (Gg.V2.v !scale' !scale') *|| m in

      set_matrix cr (Utils.Matrix.cairo_matrix_of_m3 m);

      set_source_surface cr image ~x:(~-.im_width /. 2.0) ~y:(~-.im_height /. 2.0);
      rectangle cr (~-.im_width /. 2.0) (im_height /. 2.0) (im_width -. 1.0) (~-.im_height -. 1.0);
      fill cr;

      let m_overlay = M3.scale2 (V2.v 1.0 ~-.1.0) *|| m in

      inverse_transformation_matrix := Some (Gg.M3.inv m_overlay);

      set_matrix cr (cairo_matrix_of_m3 m_overlay);

      let bounds' = bounds (M3.inv m) (0.0, 0.0) (area_width, area_height) in
      let context = object
	  method cairo = cr
	  method bounds = bounds'
      end in
      Hook.issue overlay context
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
    method set_image_position v = image_position := v
    method on_button_press = on_button_press
    method on_mouse_move = on_mouse_move
    method overlay = overlay
    method set_angle a = angle := a
    method get_scale = !scale'
    method set_scale s = scale' := s
  end in
  let button_pressed ev =
    ( match !inverse_transformation_matrix with
    | None -> ()
    | Some matrix ->
      let (x, y) = (GdkEvent.Button.x ev, GdkEvent.Button.y ev) in
      let xy = Gg.P2.tr matrix (Gg.V2.v x y) in
      ignore (Hook.issue on_button_press xy);
      ()
    );
    true
  in
  let mouse_moved ev =
    ( match !inverse_transformation_matrix with
    | None -> ()
    | Some matrix ->
      let (x, y) = (GdkEvent.Motion.x ev, GdkEvent.Motion.y ev) in
      let xy = Gg.P2.tr matrix (Gg.V2.v x y) in
      ignore (Hook.issue on_mouse_move xy);
      ()
    );
    true
  in
  let mouse_scroll ev =
    let _ =
      scale' := !scale' *.
	match GdkEvent.Scroll.direction ev with
	| `UP -> 1.5
	| `DOWN -> 1.0 /. 1.5
	| _ -> 1.0
    in
    true
  in
  ignore (drawing_area#event#connect#expose expose);
  drawing_area#event#add [`EXPOSURE];
  ignore (drawing_area#event#connect#button_press button_pressed);
  ignore (drawing_area#event#connect#motion_notify mouse_moved);
  ignore (drawing_area#event#connect#scroll mouse_scroll);
  drawing_area#event#add [`BUTTON_PRESS];
  drawing_area#event#add [`SCROLL];
  drawing_area#event#add [`POINTER_MOTION];
  drawing_area#misc#draw None;
  interface
