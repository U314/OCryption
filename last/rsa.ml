open Big_int

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
	
  method string_of_key = function
    | Value (v) -> string_of_big_int v
    | Notdef -> "Not available."
    | Key (e, n) -> "{\n" ^ self#string_of_key e ^ "\n" ^ self#string_of_key n ^ "\n}\n"

  method build_key p q = match sub_big_int p q with
    | n when n >= zero_big_int 
	-> self#update_key p q
    | _ -> self#update_key q p 

  method update_key p q =   
    public_key <- self#gen q (succ_big_int p);
    private_key <- self#genp q (self#get_ek public_key) p;
    
  method display_key =
    print_string (self#string_of_key public_key);
    print_string (self#string_of_key private_key);

  method give_key = function
    | "pri" -> private_key
    | "public" -> public_key
    | _ -> failwith "Invalid data."

  method get_key = function
    | Key(Value(k), Value(_)) -> (k, n)
    | _ -> failwith "Invalid data."

  method get_n = function
    | Key(Value(k), Value(_))-> n
    | _ -> failwith "Invalid data."
   
  method rsa key = function
    | data::next ->
	let tmp_data = big_int_of_string data in
	let k = self#get_ek (self#give_key key)in
	let n = self#get_n (self#give_key key) in
	let out = string_of_big_int (mod_big_int (power_big_int_positive_big_int (tmp_data) k) n) in
	out :: self#rsa key next
    | [] -> []
    
end

let _ =
  begin
    print_string "Enter a prime number: ";
    let p = big_int_of_string (read_line()) in
    print_string "Enter a second prime number: ";
    let q = big_int_of_string (read_line()) in
    print_endline "Building an instance of a rsa object.";
    let rsa = new rsa p q in
    print_string "Generating keys ...";
    rsa#build_key p q;
    print_endline " Done.";
    rsa#display_key;
    print_endline "Test:";
    let test = "42"::"42"::"42"::[] in
    List.iter print_endline test;
    print_string "Encrypting Test ...";
    let chf = rsa#rsa "public" test in
    print_endline " Done.";
    List.iter print_endline chf;
    print_string "Decrypting Test ...";
    let dchf = rsa#rsa "pri" chf in
    print_endline " Done.";
    List.iter print_endline dchf;
  end
