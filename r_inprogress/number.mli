module type NUMBER =
sig
  
  type t

  val zero : t
  val one : t

  val of_string : string -> t
  val of_int : int -> t
  val print : t -> unit
  val print_endl : t -> unit
  val to_string : t -> string

  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t
  val mul : t -> t -> t
  val exp : t -> t -> t
  val sqrt : t -> t

  val gt : t -> t -> bool
  val ge : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val ne : t -> t -> bool
  val eq : t -> t -> bool

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val ( <) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( != ) : t -> t -> bool
  val ( == ) : t -> t -> bool

  val is_prime_naif : t -> bool
  val all_pr_prime : t * t -> (t -> bool) -> t list
  val pr_prime_ctor : t * t -> (t -> bool) -> t
  val pgcd : t -> t -> t	

  type key
  val public_key_ctor : t -> t -> key
  val private_key_ctor : t -> t -> t -> key
  val print_k : key -> unit
  val rsa : key -> t list -> t list

end

(* Here is a version which use bc to make operations Exponentiel perform runtime error *)  
module Vast : NUMBER

