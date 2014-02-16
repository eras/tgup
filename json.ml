include Yojson.Safe

let rec ( @-> ) assoc key =
  match (assoc : json option) with
  | None -> None
  | Some (`Assoc assoc) ->
    ( match
	try Some (List.assoc key assoc)
	with Not_found -> None
      with 
      | None -> None
      | Some json -> Some json )
  | Some _ -> None

let get_assoc json =
  match json with
  | `Assoc x -> x
  | _ -> raise Not_found

let get_list json =
  match json with
  | `List l -> l
  | _ -> raise Not_found

let get_string json =
  match json with
  | `String l -> l
  | _ -> raise Not_found

let get_int json =
  match json with
  | `Int l -> l
  | _ -> raise Not_found

let get_linenumber (`Result result) = 
  try Some (get_int (List.assoc "n" result))
  with Not_found -> None

let get_tinyg str =
  try 
    ( let a = get_assoc (from_string str) in
      try `Result (get_assoc (List.assoc "r" a))
      with Not_found -> 
	try `Status (get_assoc (List.assoc "sr" a))
	with Not_found ->
	  try `Queue_report (get_int (List.assoc "qr" a))
	  with Not_found -> `Other a )
  with Yojson.Json_error _ ->
    `Parse_error

