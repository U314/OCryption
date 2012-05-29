module type CRYPTAGE = 
sig

  val pgcd : Big_int.big_int * Big_int.big_int -> Big_int.big_int
  val phi :Big_int.big_int * Big_int.big_int -> Big_int.big_int
  val mk_public_key : Big_int.big_int * Big_int.big_int -> Big_int.big_int * Big_int.big_int
  val mk_private_key : Big_int.big_int * Big_int.big_int * Big_int.big_int -> Big_int.big_int -> Big_int.big_int * Big_int.big_int
  val print_key : Big_int.big_int * Big_int.big_int -> string
  val get_ed : Big_int.big_int * Big_int.big_int -> Big_int.big_int
  val get_n :  Big_int.big_int * Big_int.big_int -> Big_int.big_int
  val rsa :  Big_int.big_int * Big_int.big_int -> string list -> string list
  val get_pub : Big_int.big_int * Big_int.big_int -> string
  val get_N :  Big_int.big_int * Big_int.big_int -> string
  val naif : Big_int.big_int -> bool
  val get_all_primary_number_between : (Big_int.big_int -> bool) -> Big_int.big_int * Big_int.big_int -> Big_int.big_int list
  val random_pick : string * string -> Big_int.big_int

end

module Rsa : CRYPTAGE
