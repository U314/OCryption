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


end

module Rsa : CRYPTAGE =
struct

  let rec pgcd (a, b) = 
    (* let r = *) Big_int.gcd_big_int a b(* mod_big_int a b in *)
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

end
