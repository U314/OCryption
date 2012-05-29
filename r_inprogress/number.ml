(* Here is a version which use bc to make operations Exponentiel perform runtime error *)  

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

module Vast : NUMBER =
struct

  type t = string

  let of_string str = str
  let of_int n = string_of_int n
  let print n = print_string n
  let print_endl n = print_endline n
  let to_string n = n
  let zero = of_int 0
  let one = of_int 1
    
  let op lhs rhs symbole = 
    let command = "echo \"" ^ lhs ^ symbole ^ rhs ^ "\" | bc  " in
    let rec cleaner = function
      | (i, str) when i != String.length str ->
	  if String.get str i != '\\' then
	    String.make 1 (String.get str i) ^ cleaner (i+1, str)
	  else cleaner (i + 1, str)
      | _ -> ""
    in of_string (cleaner (0, Tool.Tool.exec_shell_command command))
	 
  let booler lhs rhs symbole = 
    if String.compare (op lhs rhs symbole) "1" == 0 then true
    else false

  let print_bool = function
    | true -> print_endline "TRUE"
    | false -> print_endline "FALSE"

  let add n1 n2 =
    (op n1 n2 " + ")
      
  let sub n1 n2 =
    (op n1 n2 " - ")
      
  let div n1 n2 =
    (op n1 n2 " / ")

  let modulo n1 n2 =
    (op n1 n2 " % ")
      
  let mul n1 n2 =
    (op n1 n2 " * ")
        
  let exp n1 n2 =
    if n2 < of_string "999999999" then
      (op n1 n2 " ^ ")
    else failwith "Exp: is too large... Sorry"
      
  let sqrt n1 =
    (op ("sqrt ( " ^  n1 ^ " )") "" "")
      
  let gt n1 n2 =
    (booler n1 n2 " > ")
      
  let ge n1 n2 =
    (booler n1 n2 " >= ")
      
  let lt n1 n2 =
    (booler n1 n2 " < ")

  let le n1 n2 =
    (booler n1 n2 " <= ")

  let eq n1 n2 =
    (booler n1 n2 " == ")
      
  let ne n1 n2 =
    (booler n1 n2 " != ")

  let ( + ) lhs rhs = 
    add lhs rhs

  let ( - ) lhs rhs = 
    sub lhs rhs

  let ( * ) lhs rhs = 
    mul lhs rhs

  let ( / ) lhs rhs = 
    div lhs rhs

  let ( mod ) lhs rhs = 
    modulo lhs rhs

  let ( ** ) lhs rhs = 
    exp lhs rhs

  let ( < ) lhs rhs = 
    lt lhs rhs

  let ( <= ) lhs rhs = 
    le lhs rhs

  let ( > ) lhs rhs = 
    gt lhs rhs
      
  let ( >= ) lhs rhs = 
    ge lhs rhs

  let ( == ) lhs rhs = 
    eq lhs rhs

  let ( != ) lhs rhs = 
    ne lhs rhs

  let is_prime_naif n = 
    let limit = sqrt n in
    let rec loop = function
      | i when i < limit -> 
	  if n mod i == zero then false
	  else loop (i + one)
      | _ -> true
    in loop (of_int 2)

  let all_pr_prime (a, b) f =
    let rec loop = function
      | n when n < b -> 
	  if f n then  n :: loop (n + one)
	  else loop (n + one)
      | _ -> []
    in 
    if a > of_int 8 then loop a
    else failwith "This is for big Number.t ..."

  let pr_prime_ctor (a, b) f =
    let lprime = all_pr_prime (a, b) f in
    if (of_int (List.length lprime) < of_int (2) ** of_int (31)) then
      List.nth lprime (Random.int (List.length lprime))
    else failwith "Range is too large"

  let rec pgcd a b =
    let r = a mod b in
    if r == zero then  b
    else pgcd b r


  type key = {key : t; n : t}
      
  let phi p q =
    (p - one) * (q - one)
      
  let public_key_ctor p q =
    let n = p * q in
      let phi = phi p q in
      let rec find_key = function
	| k when k < phi && pgcd k phi == one
	    -> k
	| k -> find_key ( k + one)
      in 
      if p < q then { key = find_key (q + one); n = n }
      else { key = find_key (p + one); n = n}
	
  let print_k key = 
    print_endline (to_string (key.key) ^ "\n" ^ to_string (key.n))
      
      
  let private_key_ctor p q pubkey =
    let phi = phi p q in
    let n = p * q in
    let rec privkctor pubkey = function
      | k when k < phi
  	  ->
	  (
	    match ((pubkey * k) mod phi == one) with
	      | true ->  print_endline "Done:";{ key = k; n = n }
	      | false -> privkctor pubkey (k + one)
	  )
      | _ -> failwith "Generating private key failed: not found."
    in privkctor pubkey pubkey


  let rec rsa key = function
    | d::n ->
	to_string ((d ** key.key) mod key.n) :: rsa key n
    | [] -> []

  let _ = 
    begin
      let p = of_string (read_line()) in
      let q = of_string (read_line()) in
      print_endline "Generating public key.";
      let pub = public_key_ctor p q in
      print_k pub;
      print_endline "Generating private key.";
      let pri = private_key_ctor p q (pub.key mod pub.n) in
      print_k pri;
      
      
    end
end
  
	  

(* let _ =  *)
  (*   begin *)
  (*     let a = of_string (read_line()) in *)
  (*     let b = of_string (read_line()) in *)
  (*     all_pr_prime (a, b ) is_prime_naif; *)
  (*     let r = (pr_prime_ctor (a, b) is_prime_naif) in *)
  (*     if is_prime_naif r then *)
  (* 	print_endl r *)
  (*     else  *)
  (* 	print_endline "FAILED" *)
	  
  (*   end *)

