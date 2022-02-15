open Bdd;;
open Draw;;

<<<<<<< HEAD
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
=======
(* (P <-> Q) <-> R *)
let f1 = BOper(BOper(Symbol 'P', Iff, Symbol 'Q'), Iff, Symbol 'R')
let b1 = bdd_of_formula f1

let _ = run b1
>>>>>>> 26a2b97ddaddaf8dc96ce0d80e80dc604eb1befb
