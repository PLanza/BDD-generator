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
  | Node of char * bdd * bdd

val string_of_op : bop -> string
val string_of_bdd : bop -> string

val eval_not : bdd -> bdd
val eval_op : bool -> bdd -> bop -> bdd

val merge_bdds : bdd -> bop -> bdd -> bdd
val create_bdd : formula -> bdd