open Batteries

let (@@) a b = a b

let pi2 = 8. *. atan 1.

let show_exn f =
  try 
    f ()
  with exn ->
    Printf.printf "Exception: %s (%s)\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ());
    raise exn

let convert_rgb_to_rgbx (width, height) rgb_data =
  let open Bigarray in
  let open Array1 in
  let stride = Cairo.Image.stride_for_width RGB24 width in
  let rgbx_data = create int8_unsigned c_layout (stride * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      for c = 0 to 2 do
	rgbx_data.{y * stride + (x * 4) + c} <-
	  rgb_data.{y * width * 3 + (x * 3) + c}
      done
    done
  done;
  (stride, rgbx_data)

let view (width, height) ?packing () =
  let drawing_area = GMisc.drawing_area ?packing ~width ~height () in
  let image = ref None in
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
      scale cr x_scale y_scale;
      set_source_surface cr image ~x:0.0 ~y:0.0;
      rectangle cr 0.0 0.0 im_width im_height;
      fill cr
  in
  let expose ev =
    show_exn @@ fun () ->
      let open Cairo in
      let cr = Cairo_gtk.create drawing_area#misc#window in
      let allocation = drawing_area#misc#allocation in
      draw cr (float allocation.Gtk.width) (float allocation.Gtk.height);
      true
  in
  ignore (drawing_area#event#connect#expose expose);
  drawing_area#event#add [`EXPOSURE];
  let interface = object
    method set_image ((width, height), rgb_data) =
      let format = Cairo.Image.RGB24 in
      let (stride, rgb_data) = convert_rgb_to_rgbx (width, height) rgb_data in
      image := Some (Cairo.Image.create_for_data8 ~stride rgb_data format width height, width, height);
      drawing_area#misc#draw None
  end in
  interface
