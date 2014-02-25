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

let transform matrix z_offset data =
  let open Gcode.Evaluate in
  let map_vector op a b =
    let ab = Gg.V2.v a b in
    let ab' = op matrix ab in
    let (a', b') = Gg.V2.to_tuple ab' in
    (a', b')
  in
  let map state =
    let (!) x = AxisMap.find x state.ms_position in
    let (x', y') = map_vector Gg.P2.tr !`X !`Y in
    let is_arc = List.mem state.ms_g_motion [`G2; `G3] in
    let ij' =
      let (!) x =
        try Some (RegNoAxisMap.find x state.ms_regs)
        with Not_found -> None
      in
      match !`I, !`J with
      | Some i, Some j -> Some (map_vector Gg.V2.tr i j)
      | Some _, None
      | None, Some _ when is_arc -> failwith "transform: partial I/J with arc not supported yet"
      | _ when is_arc -> failwith "arc requires I and J"
      | _ -> None
    in
    { state with
      ms_position =
        (let (++) m (key, value) = AxisMap.add key value m in
         state.ms_position ++ (`X, x') ++ (`Y, y') ++ (`Z, !`Z +. z_offset));
      ms_regs =
        (let (++) m (key, value) =
           match value with
           | None -> m
           | Some value -> RegNoAxisMap.add key value m
         in
         state.ms_regs ++ (`I, Option.map fst ij') ++ (`J, Option.map snd ij'));
    }
  in
  let data = data |> Enum.map @@ fun sr ->
    { sr with
      sr_state0 = map sr.sr_state0;
      sr_state1 = map sr.sr_state1; }
  in
  data
