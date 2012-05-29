

let main () = function
  | filename ->
      begin
	print_string "Enter p: ";
	let p = read_line() in
	print_string "Enter q: ";
	let q = read_line() in
	print_endline "Generating public key ...";
	let pubk = Cryptage.Rsa.mk_public_key (Big_int.big_int_of_string p, Big_int.big_int_of_string q) in
	print_endline (Cryptage.Rsa.print_key pubk);
	Tool.Utool.file_of_string (filename ^ ".pub", (Cryptage.Rsa.get_pub pubk) ^ "\n" ^ (Cryptage.Rsa.get_N pubk));
	print_endline "Generating public key done.\nGenerating private key ...";
	let prik = Cryptage.Rsa.mk_private_key (Big_int.big_int_of_string p, Big_int.big_int_of_string q, Cryptage.Rsa.get_ed pubk) (Cryptage.Rsa.get_ed pubk) in
	print_endline (Cryptage.Rsa.print_key prik);
	Tool.Utool.file_of_string (filename ^ ".pri", (Cryptage.Rsa.get_pub prik) ^ "\n" ^ (Cryptage.Rsa.get_N prik));
	print_endline "Generating private key done.";
      end

let _ = 
  if Array.length (Sys.argv) == 2 then
    main () Sys.argv.(1)
  else failwith "Use ./RSAkeygen name"
