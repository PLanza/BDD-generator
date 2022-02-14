open Bdd;;
open Draw;;

(* (P <-> Q) <-> R *)
let f1 = BOper(BOper(Symbol 'P', Iff, Symbol 'Q'), Iff, Symbol 'R')
let b1 = bdd_of_formula f1

let _ = run b1
