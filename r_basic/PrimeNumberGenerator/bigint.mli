module type BIGINT =
sig

  type bigint
  
  val bigint_of_string : string -> bigint
  val string_of_bigint : bigint -> string
  val print_bigint : bigint -> unit
  val (+) : bigint -> bigint -> bigint
  val (-) : bigint ->  bigint -> bigint
  val (*) : bigint ->  bigint -> bigint
  val (/) : bigint -> bigint -> bigint
  val (mod) : bigint -> bigint -> bigint
  val ( ** ) : bigint -> bigint -> bigint

end

module Bigint : BIGINT
