open Batteries

let enum_unfold_map xs v0 f =
  BatEnum.unfold
    v0
    (fun v -> 
      match BatEnum.get xs with
      | None -> None
      | Some x -> Some (f v0 x)
    )

let coalesce2 a b =
  match a with
    | None -> b
    | Some _ -> a

let transform matrix data =
  let open Gcode.Parser in
  let module State = struct
      type partial = {
	x : float option;
	y : float option;
      }
      type complete = {
	at : Gg.V2.t;
      }
      type t =
      | Partial of partial
      | Complete of complete
  end in
  let open State in
  let complete_state state new_position =
    let get_opt key =
      try Some (AxisMap.find key new_position)
      with Not_found -> None
    in
    let get key default = Option.default default (get_opt key) in
    match state with
    | Complete complete ->
      let open Gg.V2 in
      Complete { at = v (get `X @@ x complete.at) (get `Y @@ y complete.at) }
    | Partial partial ->
      let x = coalesce2 (get_opt `X) (partial.x) in
      let y = coalesce2 (get_opt `Y) (partial.y) in
      match x, y with
      | None, _ | _, None -> Partial { x; y }
      | Some x, Some y -> Complete { at = Gg.V2.v x y }
  in
  let mapping state = function
    | Move ((G0 | G1) as command, coords, rest) ->
      ( match complete_state state coords with
      | Partial _ ->
        (* ignore all partial information *)
        (* TODO: NOTE! this will also have removed Z transitions! *)
        ([], state)
      | Complete complete ->
        let (+@) map (k, v) = AxisMap.add k v map in
        let at = Gg.P2.tr matrix complete.at in
        ([Move (command, AxisMap.empty +@ (`X, Gg.V2.x at) +@ (`Y, Gg.V2.y at), rest)], state)
      )
    | word -> ([word], state)
  in
  let data = enum_unfold_map data (Partial { x = None; y = None }) mapping |> Enum.map List.enum |> Enum.concat in
  let data = enum_unfold_map data default_machine_state @@ fun machine_state x -> string_of_input ~machine_state x in
  Enum.concat
    (List.enum
       [Enum.singleton "; Mangled with gcode-leveler https://github.com/eras/gcode-leveler\n";
        data])
