
let key_retreiver = function
  | filename -> 
      let content = Tool.Utool.string_of_file filename in
      if String.contains content '\n' then 
	let isep = String.index content '\n' in
	let key = String.sub content 0 isep in
	let n = String.sub content isep (String.length content - isep) in
	(Big_int.big_int_of_string key, Big_int.big_int_of_string n)
      else
	failwith "File is corrupt."
	  
let main = function
  | (pri, pub) ->
      let enc = key_retreiver pub in
      let dec = key_retreiver pri in
      begin
	print_endline (Cryptage.Rsa.print_key enc);
	print_endline (Cryptage.Rsa.print_key dec);
	print_string "Enter pathname of a file to crypt: ";
	let pathname = read_line () in
	print_string "Enter pathname for the output file: ";
	let destname = read_line () in
	print_string "Enter positive number to encrypt or a negative number to decrypt: ";
	let op = int_of_string (read_line ()) in
	let content = Tool.Utool.string_of_file pathname in
	if op == 0 then
	  let lc = Tool.Utool.get_asci_list (0 , content) in
	  let leenc = Cryptage.Rsa.rsa enc lc in
	  Tool.Utool.write_list_into (Tool.Utool.open_for_write destname) leenc;
	else
	  let lc = Tool.Utool.get_list_of_file pathname in
	  let p = Unix.fork () in
	  let f x = () in
	  if p == 0 then
	    let deenc = List.rev (Cryptage.Rsa.rsa dec lc) in
	    Tool.Utool.write_list_into_2 (Tool.Utool.open_for_write destname) deenc;
	    exit 0
	  else 
	    f (Unix.wait ())
      end
	
let _ = 
  if Array.length Sys.argv == 3 then
    main (Sys.argv.(1), Sys.argv.(2))
  else 
    print_endline "/!\\ Use: ./RSAcrypter pathname1 pathname2;\nWhere pathname1 is your private key's file and pathname2 is your public key's file."
  

(* lance avec pub key filename prikey filename *)
(* recupere les cles / lancement d'un ligne de commande / commande possible (keyfilepath <=> history) enc... dec ... *)
