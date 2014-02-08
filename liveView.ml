open Batteries

let (@@) a b = a b

let pi2 = 8. *. atan 1.

let show_exn f =
  try 
    f ()
  with exn ->
    Printf.printf "Exception: %s (%s)\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ());
    raise exn

let convert_stride stride (width, height) rgb_data =
  if stride = width * 3
  then rgb_data
  else
    let open Bigarray in
    let open Array1 in
    let rgb_data_out = create int8_unsigned c_layout (stride * height) in
    for y = 0 to height - 1 do
      blit
	(sub rgb_data (y * 3 * width) (3 * width))
	(sub rgb_data_out (y * stride) (3 * width))
    done;
    rgb_data_out


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
      let stride = Cairo.Image.stride_for_width format width in
      let rgb_data = convert_stride stride (width, height) rgb_data in
      Printf.printf "%d,%d %d\n%!" width height (Bigarray.Array1.dim rgb_data);
      image := Some (Cairo.Image.create_for_data8  ~stride rgb_data format width height, width, height);
      drawing_area#misc#draw None
  end in
  (* let count = ref 0 in  *)
  (* let received_data (data : BoundaryDecoder.data) = *)
  (*   show_exn @@ fun () -> *)
  (*     let content_length = int_of_string (List.assoc "Content-Length" data.data_header) in *)
  (*     (\* Printf.printf "Received data (%d/%d bytes)\n%!" (String.length data.data_content) content_length; *\) *)
  (*     if save_images then ( *)
  (* 	let filename = Printf.sprintf "output/%04d.jpg" !count in *)
  (* 	incr count; *)
  (* 	output_file ~filename ~text:data.data_content; *)
  (*     ); *)
  (*     match Jpeg.decode_int Jpeg.rgb4 (Jpeg.array_of_string data.data_content) with *)
  (*     | Some jpeg_image -> *)
  (* 	let (width, height) = (jpeg_image.Jpeg.image_width, jpeg_image.Jpeg.image_height) in *)
  (* 	let rgb_data = jpeg_image.Jpeg.image_data in *)
  (* 	image := Some (Cairo.Image.create_for_data8 rgb_data Cairo.Image.RGB24 width height, width, height); *)
  (* 	drawing_area#misc#draw None *)
  (*     | None -> *)
  (* 	() *)
  (* in *)
  interface

(* let main () = *)
(*   let http_mt = make_http_mt () in *)
(*   let main_window = GWindow.window ~border_width:10 () in *)
(*   Gobject.set GtkBaseProps.Window.P.allow_shrink main_window#as_window true; *)
(*   ignore (main_window#connect#destroy ~callback:destroy); *)
(*   let vbox = GPack.vbox ~packing:main_window#add () in *)
(*   let quit_button = GButton.button ~label:"Quit" ~packing:(vbox#pack ~expand:false) () in *)
(*   ignore (quit_button#connect#clicked ~callback:destroy); *)
(*   let urls = read_streams () in *)
(*   List.iter (fun url -> ignore (view url http_mt ~packing:vbox#add ())) urls; *)
(*   main_window#show (); *)
(*   GMain.Main.main () *)

(* let _ = main () *)
