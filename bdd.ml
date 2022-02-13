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

let bdd_map = 
  let h = Hashtbl.create 20 in
  Hashtbl.add h (Leaf true) (ref (Leaf true));
  Hashtbl.add h (Leaf false) (ref (Leaf false));
  h

 
let get_ref bdd = 
  Hashtbl.find bdd_map bdd

let string_of_op = function 
  | Or -> "v"
  | And -> "^"
  | Implies -> "->"
  | Iff -> "<->"
  | Xor -> "x"

let rec string_of_bdd bdd =
  match !bdd with 
  | Leaf true -> "(1)"
  | Leaf false -> "(0)"
  | Node (c1, b1, b2) -> "(" ^ Char.escaped c1 ^ " => " ^ (string_of_bdd b1) ^ " | " ^ (string_of_bdd b2) ^ ")"


let rec eval_not bdd = 
  match !bdd with 
  | Leaf true -> get_ref (Leaf false)
  | Leaf false -> get_ref (Leaf true)
  | Node (c1, b1, b2) -> 
    let n = Node (c1, eval_not b1, eval_not b2) in
    Hashtbl.add bdd_map n (ref n);
    ref n

let eval_op b bdd op =
  match op with 
  | Or -> 
    if b then
      get_ref (Leaf true)
    else 
      bdd
  | And -> 
    if b then 
      bdd
    else 
      get_ref (Leaf false)
  | Implies ->
    if b then
      bdd
    else 
      get_ref (Leaf true)
  | Iff -> 
    if b then
      bdd
    else 
      eval_not bdd
  | Xor ->
    if b then
      eval_not bdd
    else 
      bdd

let rec merge_bdds b1 op b2 =
  match !b1 with 
  | Leaf true -> eval_op true b2 op
  | Leaf false -> eval_op false b2 op
  | Node (c1, b11, b12) -> 
    (match !b2 with 
    | Leaf true -> eval_op true b1 op
    | Leaf false -> eval_op false b2 op
    | Node (c2, b21, b22) -> 
      if c1 < c2 then
        let n = Node (c1, merge_bdds b11 op b2, merge_bdds b12 op b2) in
        Hashtbl.add bdd_map n (ref n);
        get_ref n
      else if c2 > c1 then
        let n = Node (c2, merge_bdds b21 op b1, merge_bdds b22 Or b1) in
        Hashtbl.add bdd_map n (ref n);
        get_ref n
      else 
        let n = Node (c1, merge_bdds b11 op b21, merge_bdds b12 op b22) in
        Hashtbl.add bdd_map n (ref n);
        get_ref n
    )

let rec create_bdd formula =
  match formula with 
  | True -> get_ref (Leaf true)
  | False -> get_ref (Leaf false)
  | Symbol c -> 
    let n = Node (c, get_ref (Leaf true), get_ref (Leaf false)) in
    Hashtbl.add bdd_map n (ref n);
    get_ref n
  | UOper (Not, f) -> eval_not (create_bdd f)
  | BOper (f1, op, f2) -> 
    (match f1 with 
    | Symbol c1 -> 
      (match f2 with 
      | Symbol c2 -> merge_bdds (create_bdd (Symbol c1)) op (create_bdd (Symbol c2))
      | True -> merge_bdds (create_bdd (Symbol c1)) op (get_ref (Leaf true))
      | False -> merge_bdds (create_bdd (Symbol c1)) op (get_ref (Leaf false))
      | f2 -> merge_bdds (create_bdd (Symbol c1)) op (create_bdd f2)
      )
    | True -> merge_bdds (get_ref (Leaf true)) op (create_bdd f2)
    | False -> merge_bdds (get_ref (Leaf false)) op (create_bdd f2)
    | f1 -> merge_bdds (create_bdd f1) op (create_bdd f2)
    )

let f1 = BOper (BOper (Symbol 'P', Iff, Symbol 'Q'), Iff, Symbol 'R')
let b1 = create_bdd f1 ;;

let f2 = BOper (Symbol 'P', And, BOper (Symbol 'Q', Iff, Symbol 'R'))
let b2 = create_bdd f1 ;;
Printf.printf "%s\n" (string_of_bdd b1) ;;
Printf.printf "%s\n" (string_of_bdd b2) ;;