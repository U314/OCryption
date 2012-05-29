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

module Utool : TOOL

