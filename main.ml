open Bdd;;
open Draw;;

let f1 = BOper(BOper(Symbol 'P', Implies, Symbol 'R'), And, BOper(Symbol 'Q', Implies, Symbol 'R'))
let b1 = create_bdd f1

let _ = run b1