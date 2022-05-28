type uop =
  | Not

type bop =
  | Or
  | And
  | Implies
  | Iff
  | Xor

type formula =
  | Symbol of char
  | UOper of uop * formula
  | BOper of formula * bop * formula
  | True
  | False

type bdd =
  | Leaf of bool
  | Node of char * bdd ref * bdd ref

val bdd_map : (bdd, bdd ref) Hashtbl.t
val get_ref : bdd -> bdd ref
val new_bdd : bdd -> bdd ref

val string_of_op : bop -> string
val string_of_formula : formula -> string
val string_of_bdd : bdd ref -> string

val eval_not : bdd ref -> bdd ref
val eval_op : bool -> bdd ref -> bop -> int -> bdd ref

val merge_bdds : bdd ref -> bop -> bdd ref -> bdd ref
val create_bdd : formula -> bdd ref

val remove_redundancy : bdd ref -> bdd ref
val bdd_of_formula : formula -> bdd ref
