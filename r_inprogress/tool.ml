module type TOOL =
sig

  val input_lines : in_channel -> string list
  val string_of : string list -> string
  val exec_shell_command : string -> string

end

module Tool : TOOL =
struct

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
    | [] -> []
    | line -> line @ input_lines file

let rec string_of = function
  | d::n -> d ^ string_of n
  | [] -> ""

let exec_shell_command = function
  | str -> 
      let ic = Unix.open_process_in str in
      let ls = input_lines ic in
      begin
	ignore (Unix.close_process_in ic);
	string_of ls
      end

end
