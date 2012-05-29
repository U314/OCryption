module type TOOL =
sig

  val input_lines : in_channel -> string list
  val string_of : string list -> string
  val exec_shell_command : string -> string

end

module Tool : TOOL
