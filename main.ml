open Bdd;;
open Draw;;

(* ((-P & -Q & -R) | (P & Q) | (Q & R)) *)
let f1 = BOper(BOper(
  BOper(BOper(UOper(Not, Symbol 'P'), And, UOper(Not, Symbol 'Q')), And, UOper(Not, Symbol 'R')),
  Or, 
  BOper(Symbol 'P', And, Symbol 'Q')),
  Or,
  BOper(Symbol 'Q', And, Symbol 'R'))
let b1 = bdd_of_formula f1

let _ = 
	Printf.printf "%s\n" (string_of_bdd b1);
	run b1