module type FILETOOL =
sig
  
  val input_channel_of : string -> in_channel
  val input_lines_of : in_channel -> string list
  val close_input_channel : in_channel -> unit
  val output_channel_to : string -> out_channel
  val list_to_output_channel : out_channel -> string list -> unit
  val close_output_channel : out_channel -> unit

end

module Filetool : FILETOOL =
struct

  let input_channel_of file =
    try open_in file
    with Sys_error e -> (prerr_endline e; exit 255)

  let rec input_lines_of ic =
    let line = try [input_line ic] with End_of_file -> [] in
    match line with
      | [] -> [] 
      | _ -> line @ (input_lines_of ic)
	  
  let close_input_channel ic =
    try close_in ic 
    with Sys_error e -> (prerr_endline e; exit 255)

  let output_channel_to file =
    try open_out file
    with Sys_error e -> (prerr_endline e; exit 255)

  let rec list_to_output_channel oc = function
    | d::n -> 
	output_string oc d;
	list_to_output_channel oc n
    | [] -> ()
    
  let close_output_channel oc =
    try close_out oc 
    with Sys_error e -> (prerr_endline e; exit 255)
    
end
