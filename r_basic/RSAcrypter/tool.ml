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
  val get_asci_list : int * string -> string list
  val string_of_list_spe : string list -> string
  val write_list_into : fd -> string list -> unit
  val get_list_of_file :  string -> string list
  val write_list_into_2 : fd -> string list -> unit

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

  let rec write_list_into fd = function
    | (d::n) -> 
	let f x = () in 
	begin
	  f (Unix.write fd (d ^ "\n") 0 (String.length d + 1));
	  write_list_into fd n
	end
    | [] -> ()

let rec write_list_into_2 fd = function
    | (d::n) -> 
	let f x = () in 
	begin
	  if int_of_string d < 255 then
	    f (Unix.write fd (String.make 1 (char_of_int (int_of_string  d))) 0 1)
	  else ();
	  write_list_into_2 fd n
	end
    | [] -> ()
	  
		  
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
    | d::n -> d ^string_of_list n
    | [] -> ""

  let rec get_asci_list = function
    | (i, str) when i != String.length str -> 
	begin
	  (string_of_int (int_of_char (String.get str i))) :: get_asci_list (i+1, str)
	end
    | _ -> []

  let rec string_of_list_spe = function
    | d::n -> d ^ "\n" ^ string_of_list n
    | [] -> ""

  let get_list_of_file = function
    | filename ->
	let ic = open_in filename in
	let rec aux acc =
	  try
	    let line = input_line ic in
	    let acc =
	      line :: acc
            in
	    aux acc
	  with End_of_file ->
	    begin
	      close_in ic;
	      acc;
	    end
	in aux [] 
end
