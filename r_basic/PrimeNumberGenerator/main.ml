


let main = function
  | (s, e) ->
      begin
	print_endline (Big_int.string_of_big_int (Cryptage.Rsa.random_pick (s, e)))
      end

let _ = 
  if Array.length (Sys.argv) == 3 then
    main (Sys.argv.(1), Sys.argv.(2))
  else failwith "Use ./PrimeNumberGenerator bounds bounds"
