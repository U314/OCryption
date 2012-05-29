module type RSA =
sig
  type t
  type key 
    
  val public_key_ctor : t -> t -> key
  val private_key_ctor : t ->  t -> t -> t -> key
  val phi : t -> t -> t
  val print : key -> unit
    
end

module Rsa : RSA
