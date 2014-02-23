class type video = object
    method get_fd : Unix.file_descr
    method get_frame : V4l2.frame
    method start : unit -> unit
    method stop : unit -> unit
end

class v4l2 device : video = object
    val v4l2 = V4l2.init device { width = 640; height = 480 } 
    method get_fd = V4l2.get_fd v4l2
    method get_frame = V4l2.get_frame v4l2
    method start () = V4l2.start v4l2
    method stop () = V4l2.stop v4l2
end

class null : video = object
    val pipe = Unix.pipe ()
    method get_fd = fst pipe
    method get_frame = object
      method raw = ""
      method rgb = ""
      method raw_ba = Bigarray.(Array1.(create int8_unsigned c_layout 0))
      method rgb_ba = Bigarray.(Array1.(create int8_unsigned c_layout 0))
    end
    method start () = ()
    method stop () = ()
end

