type gNode = char * (int * int)

val node_map : (Bdd.bdd ref, gNode) Hashtbl.t
val get_node : Bdd.bdd ref -> gNode
val new_node : Bdd.bdd ref -> gNode -> gNode

val width : int
val height : int

val draw_node : gNode -> unit
val connect_nodes : (int * int) -> (int * int) -> (int * int) -> unit

val draw_bdd : Bdd.bdd ref -> (int * int) -> (int * int)

val run : Bdd.bdd ref -> unit