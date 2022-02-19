open Bdd;;
open Draw;;

let init_lexbuf file =
	let in_chan =
		try open_in file
		with _ -> Error.complain ("\nCan't open file" ^ file)
	in let lexbuf = Lexing.from_channel in_chan
	in (file, lexbuf)

let parse (file, lexbuf) =
	let formula = Parser.start Lexer.token lexbuf
	in let _ = Printf.printf "Parsed formula: %s\n" (Bdd.string_of_formula formula)
	in (file, formula)

let _ = 
	let (_, formula) = parse (init_lexbuf "./example.bdd") in
	let bdd = bdd_of_formula formula in
	Printf.printf "%s\n" (string_of_bdd bdd);
	run bdd
	
