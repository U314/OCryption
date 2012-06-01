module type FILETOOL =
sig
  
  val input_channel_of : string -> in_channel
  val input_lines_of : in_channel -> string list
  val close_input_channel : in_channel -> unit
  val output_channel_to : string -> out_channel
  val list_to_output_channel : out_channel -> string list -> unit
  val close_output_channel : out_channel -> unit

end

module Filetool : FILETOOL

