module type RSA =
sig
  type t
  type key 
    
  val public_key_ctor : t -> t -> key
  val private_key_ctor : t -> t -> t -> t-> key
  val phi : t -> t -> t
  val print : key -> unit
    
end

module Rsa : RSA =
struct
  type t = Number.Vast.t
  type key = { key : t; n : t }

  let public_key_ctor p q =
    let n = Number.Vast.mul p q in
    let phi = Number.Vast.mul (Number.Vast.sub p Number.Vast.one) (Number.Vast.sub q Number.Vast.one) in
    let rec find_key = function
      | k when Number.Vast.lt k phi && Number.Vast.eq (Number.Vast.pgcd k phi) Number.Vast.one
	  -> k
      | k -> find_key (Number.Vast.add k Number.Vast.one)
    in 
    if Number.Vast.lt p q == true then { key = find_key q; n = n }
    else { key = find_key p; n = n}
	  
  let print key = 
    print_endline (Number.Vast.to_string (key.key) ^ "\n" ^ Number.Vast.to_string (key.n))

  
  let phi p q =
    Number.Vast.mul (Number.Vast.sub p Number.Vast.one) (Number.Vast.sub q Number.Vast.one)

  let rec private_key_ctor p q pubkey = function
    | k when Number.Vast.eq (Number.Vast.modulo (Number.Vast.mul pubkey k) (phi p q)) Number.Vast.one
  	-> { key = k; n = (Number.Vast.mul p q) }
    | k when Number.Vast.lt k (phi p q)
  	->
  	begin
  	  print_string (Number.Vast.to_string k ^ "\n");
  	  (* if Number.Vast.eq (Number.Vast.modulo k (Number.Vast.of_int 2 )) Number.Vast.zero *)
  	  private_key_ctor p q pubkey (Number.Vast.add k Number.Vast.one)
  	  (* else private_key_ctor p q pubkey (Number.Vast.add k (Number.Vast.of_int 2)) *)
  	end
    | _ -> failwith "Generating private key failed: not found."

  let _ = 
    begin
      let p = Number.Vast.of_string (read_line()) in
      let q = Number.Vast.of_string (read_line()) in
      let pub = public_key_ctor p q in
      print pub;
      print_endline "Generating private key will start.";
      let pri = private_key_ctor p q pub.key (Number.Vast.div pub.key p) in
      print pri;
    end
end
