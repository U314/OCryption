open Unix

(*  stdin::stdout::stderr  *)
(*   1        2       4    *)

let print = function
  | (i, _) ->
      begin
	print_endline ("pid: " ^ string_of_int i);
	i
      end

let fork_daemon service args =
  let pid = fork () in
  match pid with
    | 0 -> ignore (service args)
    | _ -> ignore (exit 0)
  
let daemon_genitor service = function
  | args ->
      let pid = fork () in
      match pid with 
	| 0 -> fork_daemon service args
	| _ -> ignore (print (wait ()))
	    
let hello_serv = function
  | str ->
      begin
	print_endline str;
	exit (1);
      end
		 	
