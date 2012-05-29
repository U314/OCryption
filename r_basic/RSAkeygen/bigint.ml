module type BIGINT =
sig

  type bigint
  
  val bigint_of_string : string -> bigint
  val string_of_bigint : bigint -> string
  val print_bigint : bigint -> unit
  val ( + ) : bigint -> bigint -> bigint
  val ( - ) : bigint ->  bigint -> bigint
  val ( * ) : bigint ->  bigint -> bigint
  val ( / ) : bigint -> bigint -> bigint
  val (mod) : bigint -> bigint -> bigint
  val ( ** ) : bigint -> bigint -> bigint

end

module Bigint : BIGINT =
struct
  
  type bigint = string
      
  let bigint_of_string = function
    | str -> str
  
  let string_of_bigint = function
    | nb -> nb

  let print_bigint = function
    | nb -> 
	begin
	  print_string nb;
	end
	  
  let ( + ) lhs rhs =
    Operations.Operations.add (lhs, rhs)

  let ( - ) lhs rhs =
    Operations.Operations.sub (lhs, rhs)
      
  let ( * ) lhs rhs =
    Operations.Operations.mul (lhs, rhs)
      
  let ( / ) lhs rhs =
    Operations.Operations.div (lhs, rhs)

  let ( ** ) lhs rhs = 
    Operations.Operations.exp (lhs, rhs)
      
  let ( mod ) lhs rhs =
    Operations.Operations.modulo (lhs, rhs)

end

