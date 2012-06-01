open Big_int
open Filetool

type key =
  | Key of key * key
  | Value of big_int
  | Notdef

let naif = function
  | n -> 
      let limit = sqrt_big_int n in
      let rec loop = function
	| i when le_big_int i limit ->
	    if eq_big_int (mod_big_int n i) zero_big_int 
	    then false
	    else loop (succ_big_int i)
	| _ -> true
      in loop (big_int_of_int 2)


let rec get_all_primary_number_between f = function
  | (s, e) when lt_big_int s e -> 
      if f s then s :: get_all_primary_number_between f (succ_big_int s, e)
      else get_all_primary_number_between f (succ_big_int s, e)
  | _ -> []
      
let random_pick = function
  | (a, b) ->
      let l = get_all_primary_number_between naif (big_int_of_string a, big_int_of_string b) in
      begin
	let r = Random.int (List.length l) in
	List.nth l r
      end


class	rsa  p q =
object (self)
  
  val mutable public_key = Notdef
  val mutable private_key = Notdef
  val mutable phi = mult_big_int (pred_big_int p) (pred_big_int q)
  val mutable n = mult_big_int p q
  val mutable _p = p
  val mutable _q = q
  val mutable extend_pub = "-rsa.public"
  val mutable extend_pri = "-rsa.private"

  method private gen p = function
    | e when lt_big_int e phi 
	&& eq_big_int (gcd_big_int e phi) unit_big_int 
	-> Key(Value(e), Value(n))
    | e -> self#gen p (succ_big_int e)

  method private get_ek = function
    | Key (Value (e), Value(_)) -> e
    | _ -> failwith "Invalid data."

  method private genp p e = function
    | d when eq_big_int (mod_big_int (mult_big_int e d) phi) unit_big_int -> Key(Value(d), Value(n)) 
    | d when lt_big_int d phi -> self#genp p e (succ_big_int d)
    | _ -> failwith "Search abort."
	
  method private string_of_key = function
    | Value (v) -> string_of_big_int v
    | Notdef -> "Not available."
    | Key (e, n) -> "{\n" ^ self#string_of_key e ^ "\n" ^ self#string_of_key n ^ "\n}\n"

  method private build_key = match sub_big_int _p _q with
    | n when n >= zero_big_int 
	-> self#update_key _p _q
    | _ -> self#update_key _q _p 

  method private update_key p q =   
    public_key <- self#gen q (succ_big_int p);
    private_key <- self#genp q (self#get_ek public_key) p;
    
  method private display_key =
    print_string (self#string_of_key public_key);
    print_string (self#string_of_key private_key);

  method private give_key = function
    | "pri" -> private_key
    | "public" -> public_key
    | _ -> failwith "Invalid data."

  method private get_key = function
    | Key(Value(k), Value(_)) -> (k, n)
    | _ -> failwith "Invalid data."

  method get_n = function
    | Key(Value(k), Value(_))-> n
    | _ -> failwith "Invalid data."
   
  method rsa key = function
    | data::next ->
	begin
	  let tmp_data = try big_int_of_string data with Failure e -> print_endline "Fucking digits";exit 255 in
	  let k = self#get_ek (self#give_key key) in
	  let n = self#get_n (self#give_key key) in
	  let out = string_of_big_int (mod_big_int (power_big_int_positive_big_int (tmp_data) k) n) in
	  (out ^ "\n") :: self#rsa key next
	end
    | [] -> []

  method private list_of = function
    | Key(Value(k), Value(n)) -> ((string_of_big_int k ^ "\n")::(string_of_big_int n ^ "\n")::[])
    | _ -> failwith "Bad parameters"

  method private key_of = function
    | k::n::[] -> Key(Value(big_int_of_string k), Value(big_int_of_string n))
    | _ -> failwith "File is corrupt"
	
  method save_key filename =
    let oc = Filetool.output_channel_to (filename ^ extend_pub) in
    Filetool.list_to_output_channel oc (self#list_of public_key);
    Filetool.close_output_channel oc;
    let oc = Filetool.output_channel_to (filename ^ extend_pri) in
    Filetool.list_to_output_channel oc (self#list_of private_key);
    Filetool.close_output_channel oc;

  method load_key_of filename =
    let ic = Filetool.input_channel_of (filename ^ extend_pub) in
    public_key <- self#key_of (Filetool.input_lines_of ic);
    Filetool.close_input_channel ic;
    let ic = Filetool.input_channel_of (filename ^ extend_pri) in
    private_key <- self#key_of (Filetool.input_lines_of ic);
    Filetool.close_input_channel ic;

  method private init = function
    | d::n -> 
	let rec s = function
	  | (i, str) when i != String.length str -> 
	      string_of_int (int_of_char (String.get str i)) :: s (i+1, str)
	  | _ -> []
	in 
	s (0, d ) @ self#init n
    | [] -> []

  method encrypt ic oc =
    let lget = Filetool.input_lines_of ic in
    let lgood = self#init lget in
    Filetool.list_to_output_channel oc (self#rsa "public" lgood);
    
  method private rinit = function
    | d::n -> 
	let tmp = String.sub d 0 (String.length d - 1) in
	let c = String.make 1 (char_of_int (int_of_string tmp)) in
	print_string c;
	c::self#rinit n;
    | [] -> []

  method decrypt ic oc =
    let lget = Filetool.input_lines_of ic in
    let dec = (self#rsa "pri" lget) in
    Filetool.list_to_output_channel oc (self#rinit dec);
  
  method get_p = _p
  method get_q = _q
  method get_extend_pri = extend_pri
  method get_extend_pub = extend_pub
  method get_public_key = self#get_key public_key
  method get_private_key = self#get_key private_key
  method random_prime_number =
    _p <- random_pick (read_line(), read_line());
    _q <- random_pick (read_line(), read_line())

  method set_p p = _p <- p;self#update_key _p _q
  method set_q q = _q <- q;self#update_key _p _q
  method set_extend_pri nextend_pri = extend_pri <- nextend_pri
  method set_extend_pub nextend_pub = extend_pub <- nextend_pub
 
  initializer
    self#random_prime_number;
    self#build_key;
    self#display_key;

end

(*  -clean (null values)    *)
(*  -no-soul *)

(*   stop /quit /exit   *)
(*   set [ p | q | extend [ public | private ] | public_key  | private_key ]      *)
(*   gen [ p | q | key ]      *)
(*   save key                 *)
(*   load key                 *)
(*   display [ key | p | q ]  *)
(*   lsd pcq la on part vraiment loin "saissisez le code" [encrypt | decrypt ] [from filename ] [ to filename ]*)

(*           
crerer l'object -> build des key -> encryption decryption ->                                *)
(* set p=P q=Q  *)


let ocryption () =
  print_endline "Ocryption";
  let bloc = new rsa (big_int_of_string "0") (big_int_of_string "0") in
  ignore bloc
    

let display_help () =
  print_endline "Use: ./Ocryption [-h]";
  print_endline "-h --> display help"
    
let _ = 
  match Sys.argv with
    | [| _; "-h" |] -> display_help () 
    | _ -> ocryption ()


(*

  parser sur l'entrer standard pour executer commande ou recuperer donnee
  
  ./RSA-OCryption

lecture de l'entrer standard 
  parse l'entrer 
        start -> entry fake - dameoon mode -> only read a stop  make stop 
  set ...
  
  select .... for crypting | decrypting 
  update key
  save currrent key in filename

  ecrtirure sur sortie standard 

  string ->
  file_ori -> rsa -> file_enc -> rsa -> file_dec
  p -> q -> public_key * private_key
  Setter et getter ...
  key -> file
  file -> key

*)
