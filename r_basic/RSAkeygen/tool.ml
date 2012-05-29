module type TOOL =
sig
  type fd

  val open_for_read : string -> fd
  val open_for_write : string -> fd
  val get_file_content : fd * int -> string
  val string_of_file : string -> string
  val file_of_string : string * string -> unit
  val apply_to_dir : string -> (string -> string) * string -> unit
  val list_of_string : int * string  -> string list
  val string_of_list : string list -> string

end

module Utool : TOOL =
struct
  
  type fd = Unix.file_descr

  let open_for_read = function
    | filename -> 
	let read_flag = Unix.O_RDONLY::[] in
	Unix.openfile filename read_flag 0o640

  let open_for_write = function
    | filename ->
	let write_flag = Unix.O_WRONLY::Unix.O_CREAT::[] in
	Unix.openfile filename write_flag 0o640

  let rec get_file_content = function
    | (fd, sizebuf) ->
        let buffer = String.make sizebuf ' ' in
        let ret = Unix.read fd buffer 0 sizebuf in
        match ret with
          | ret when ret < sizebuf && ret != 0 ->
              (String.sub buffer 0 ret) ^ get_file_content (fd, sizebuf)
          | ret when ret != 0 ->
              buffer ^ get_file_content (fd, sizebuf)
          | _ -> ""

  let rec string_of_file = function
    | (filename) ->
        let fd = open_for_read filename in
        let ret = get_file_content (fd, 256) in
        begin
          Unix.close fd;
          ret
        end

  let rec file_of_string = function
    | (filename, str) ->
        let fd = open_for_write filename in
        let funit x = () in 
	begin
	  funit (Unix.write fd str 0 (String.length str));
          Unix.close fd
	end

  let apply_to_dir format = function
    | (f, dirname) ->
        let fd = Unix.opendir dirname in
	let funit x = () in 
        let rec reader = function
          | filename ->
              begin
                file_of_string (filename ^ format, (f (string_of_file filename)));
                reader (Unix.readdir fd)
              end
        in
        begin
          funit (reader (Unix.readdir fd));
          Unix.closedir fd
        end

  let rec list_of_string = function
    | (i, str) when i < String.length str ->
	(String.sub str i 1) ::	list_of_string (i + 1, str)
    | _ -> []

  let rec string_of_list = function
    | d::n -> d ^ string_of_list n
    | [] -> ""


end
