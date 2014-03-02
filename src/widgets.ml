open Batteries
open Gtk

let level_store_widget ?store name ~packing ~env () =
  let hbox = GPack.hbox ~packing () in
  let level = Option.default (ref None) store in
  let store_button = GButton.button ~packing:hbox#pack ~label:("Store " ^ name) () in
  let restore_button = GButton.button ~packing:hbox#pack ~label:("Restore " ^ name) () in
  let label = GMisc.label ~packing:hbox#pack () in
  restore_button#misc#set_sensitive false;
  let set_level level' =
    level := Some level';
    restore_button#misc#set_sensitive true;
    Printf.ksprintf label#set_text "z=%.4f" level'
  in
  ignore (store_button#connect#clicked ~callback:(fun () ->
    let level' = Gg.V3.z env#cnc_control#get_position in
    set_level level';
  ));
  Option.may set_level !level;
  ignore (restore_button#connect#clicked ~callback:(fun () ->
    let level = Option.get !level in
    let current_level = Gg.V3.z env#cnc_control#get_position in
    env#cnc_control#adjust_z (level -. current_level)
  ));
  ()

