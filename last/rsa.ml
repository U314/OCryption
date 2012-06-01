open Big_int
open Filetool

type key =
  | Key of key * key
  | Value of big_int
  | Notdef

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
    | _ -> failwith "Search abort."; exit (255)
	
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

  method private get_n = function
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

  method private key_of = function
    | k::n::[] -> Key(Value(big_int_of_string k), Value(big_int_of_string n))
    | _ -> failwith "File is corrupt"
	
  method save_key filename =
    let oc = output_channel_to (filename ^ extend_pub) in
    list_to_output_channel oc (self#list_of public_key);
    close_output_channel oc;
    let oc = output_channel_to (filename ^ extend_pri) in
    list_to_output_channel oc (self#list_of private_key);
    close_output_channel oc;

  method load_key_of filename =
    let ic = input_channel_of (filename ^ extend_pub) in
    public_key <- self#key_of (input_lines_of ic);
    close_input_channel ic;
    let ic = input_channel_of (filename ^ extend_pri) in
    private_key <- self#key_of (input_lines_of ic);
    close_input_channel ic;

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
    let lget = input_lines_of ic in
    let lgood = self#init lget in
    close_input_channel ic;
    list_to_output_channel oc (self#rsa "public" lgood);
    close_output_channel oc;

  method private rinit = function
    | d::n -> 
	let tmp = String.sub d 0 (String.length d - 1) in
	let c = String.make 1 (char_of_int (int_of_string tmp)) in
	print_string c;
	c::self#rinit n;
    | [] -> []

  method decrypt ic oc =
    let lget = input_lines_of ic in
    close_input_channel ic;
    let dec = (self#rsa "pri" lget) in
    list_to_output_channel oc (self#rinit dec);
    close_output_channel oc;

  initializer
    self#build_key;

end


let _ =
  begin
    print_string "Enter a prime number: ";
    let p = big_int_of_string (read_line()) in
    print_string "Enter a second prime number: ";
    let q = big_int_of_string (read_line()) in
    (* print_endline "Building an instance of a rsa object."; *)
    let rsa = new rsa p q in
    let ic = input_channel_of "test.txt" in
    let oc = output_channel_to "test.test" in
    rsa#encrypt ic oc;
    let ic = input_channel_of "test.test" in
    let oc = output_channel_to "output.txt" in
    rsa#decrypt ic oc;
  (* print_endline "Test:"; *)
    (* let test = "42"::"42"::"42"::[] in *)
    (* List.iter print_endline test; *)
    (* print_string "Encrypting Test ..."; *)
    (* let chf = rsa#rsa "public" test in *)
    (* print_endline " Done."; *)
    (* List.iter print_endline chf; *)
    (* print_string "Decrypting Test ..."; *)
    (* let dchf = rsa#rsa "pri" chf in *)
    (* print_endline " Done."; *)
    (* List.iter print_endline dchf; *)
    (*  une liste d'object           *)
 end
