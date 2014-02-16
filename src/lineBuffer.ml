type t = {
  t_buffer : Buffer.t;
}

let create () = {
  t_buffer = Buffer.create 1024
}

let rec find_char ch str ofs len =
  if len > 0 then
    if str.[ofs] = ch then
      Some ofs
    else
      find_char ch str (ofs + 1) (len - 1)
  else
    None

let rec append_substring t str ofs len =
  match find_char '\n' str ofs len with
    | None -> Buffer.add_substring t.t_buffer str ofs len; []
    | Some ofs' ->
	let cur_len = ofs' - ofs in
	  Buffer.add_substring t.t_buffer str ofs cur_len;
	  let c = Buffer.contents t.t_buffer in
	  let c = 
	    if String.length c > 0 && c.[String.length c - 1] = '\r' 
	    then String.sub c 0 (String.length c - 1)
	    else c
	  in
	    Buffer.reset t.t_buffer;
	    c::append_substring t str (ofs + cur_len + 1) (len - (cur_len + 1))
