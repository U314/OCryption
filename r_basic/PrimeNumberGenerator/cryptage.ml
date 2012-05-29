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
  val naif :  Big_int.big_int -> bool
  val get_all_primary_number_between : (Big_int.big_int -> bool) -> Big_int.big_int * Big_int.big_int -> Big_int.big_int list
  val random_pick : string * string -> Big_int.big_int

end

module Rsa : CRYPTAGE =
struct

  let rec pgcd (a, b) = 
    Big_int.gcd_big_int a b
    (* let r = Big_int.mod_big_int a b in *)
    (* if Big_int.eq_big_int r Big_int.zero_big_int then b *)
    (* else pgcd (b, r) *)

  let phi (p, q) = 
    let p_tmp = Big_int.sub_big_int p Big_int.unit_big_int in
    let q_tmp = Big_int.sub_big_int q Big_int.unit_big_int in
    Big_int.mult_big_int p_tmp q_tmp

  (* may take lot of time *)
  let mk_public_key (p, q) =
    let get_e (p, q, phi_n) =
      let rec g_e = function
	| e when Big_int.lt_big_int e phi_n && Big_int.eq_big_int (pgcd (e, phi_n))  (Big_int.unit_big_int) -> e
	| e -> g_e (Big_int.succ_big_int e)
      in
      if Big_int.ge_big_int p q then g_e (Big_int.succ_big_int p)
      else g_e (Big_int.succ_big_int q)
    in
    let n = Big_int.mult_big_int p q in
    let phi_n = phi (p, q) in
    (get_e (p, q , phi_n), n)
      
  (* may take lot of time *)
  let rec mk_private_key (p, q, e) = function
    | d when Big_int.eq_big_int (Big_int.mod_big_int (Big_int.mult_big_int e d) (phi (p, q))) Big_int.unit_big_int -> (d, Big_int.mult_big_int p q)
    | d when Big_int.lt_big_int d (phi (p, q)) -> mk_private_key (p, q, e) (Big_int.succ_big_int d)
    | _ -> failwith "Out of bounds"

  let print_key = fun
    (ed, n) -> "Key: (" ^ Big_int.string_of_big_int ed ^ " , " ^ Big_int.string_of_big_int n ^ ")"
      
  let get_ed (ed, n) = ed
  let get_n (ed, n) = n

  let get_pub (key, n) = Big_int.string_of_big_int key
  let get_N (ed, n) = Big_int.string_of_big_int n

  (* may take lot of time *)
  let rec rsa (ed, n) = function
    | (data::next) ->
	(Big_int.string_of_big_int (Big_int.mod_big_int (Big_int.power_big_int_positive_big_int (Big_int.big_int_of_string data ) ed ) n)) :: rsa (ed, n) next
    | [] -> []


let naif = function
  | n -> 
      let limit = Big_int.sqrt_big_int n in
      let rec loop = function
	| i when Big_int.le_big_int i limit ->
	    if Big_int.eq_big_int (Big_int.mod_big_int n i) Big_int.zero_big_int 
	    then false
	    else loop (Big_int.succ_big_int i)
	| _ -> true
      in loop (Big_int.big_int_of_int 2)


let rec get_all_primary_number_between f = function
  | (s, e) when Big_int.lt_big_int s e -> 
      if f s then s :: get_all_primary_number_between f (Big_int.succ_big_int s, e)
      else get_all_primary_number_between f (Big_int.succ_big_int s, e)
  | _ -> []
      	  
let random_pick = function
  | (a, b) ->
      let l = get_all_primary_number_between naif (Big_int.big_int_of_string a, Big_int.big_int_of_string b) in
      begin
	let r = Random.int (List.length l) in
	List.nth l r
      end


end
