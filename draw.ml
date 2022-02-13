open Graphics;;
open Bdd;;

(* Graphical node containing the character nad its location on screen *)
type gNode = char * (int * int)

(* A map from the BDD references to the on-screen node *)
let node_map = Hashtbl.create 10

(* Shortcut function for getting the graphical node from the map *)
let get_node bdd_r = Hashtbl.find node_map bdd_r

(* Shortcut function for adding a node to the map *)
let new_node bdd_r node =
  Hashtbl.add node_map bdd_r node;
  get_node bdd_r

(* Initializes graphics *)
let _ = 
	open_graph " 1024x720+448-180";
	set_color black;
	set_line_width 2

let width = size_x ()
let height = size_y ()

(* Draws a dotted line from current drawing position to the position given *)
let dotted_lineto x y =
	(* Magnitude of direction vector *)
	let g = Float.sqrt (float_of_int (x - current_x ()) ** 2. +.
                      float_of_int (y - current_y ()) ** 2.)
	in
	(* Direction vector normalized and multiplied by 5 *)
	let (dx, dy) = (int_of_float (5. *. float_of_int (x - current_x ()) /. g),
	    						int_of_float (5. *. float_of_int (y - current_y ()) /. g))
	in
	(* Signs to test if the end of the line has been reached *)
	let sign_x = if x > current_x () then (>) else (<) in
	let sign_y = if y > current_y () then (>) else (<) in
	(* Draws the dotted segments specified by the coordinates *)
	let rec aux (x1, y1) (x2, y2) =
		if sign_x x x2 && sign_y y y2 then
  		(moveto x1 y1;
  		lineto x2 y2;
  		aux (x2 + dx, y2 + dy) (x2 + 2*dx, y2 + 2*dy) )
		else 
			()
	in
		aux (current_x (), current_y ()) (current_x () + dx, current_y () + dy)
		

(* Draws a graphical node on the screen *)
let draw_node node =
	match node with 
	| (c, (x, y)) -> 
		( draw_circle x y 15;
		moveto (x-2) (y-5);
		draw_char c )

(* Draws lines connecting a parent node to its two children nodes given *)
(* their on-screen coordinates *)
let connect_nodes p_xy c1_xy  c2_xy =
	moveto ((fst p_xy) - 11) ((snd p_xy) - 11);
	lineto (fst c1_xy) ((snd c1_xy) + 15);
	moveto ((fst p_xy) + 11) ((snd p_xy) - 11);
	dotted_lineto (fst c2_xy) ((snd c2_xy) + 15)
	
(* Recursively draws a BDD at a specific location on the screen. *)
(* If the BDD has already been drawn elsewhere then we return the location *)
(* where it was drawn previously, to connect it to its parent. *)
let rec draw_bdd bdd_r (x,y) =
	match !bdd_r with
	(* Draws the '1' leaf node. Could change to be drawn at a fixed position *)
	| Leaf true -> 
		(try snd (get_node bdd_r) with
		| Not_found -> 
			draw_node (new_node bdd_r ('1', (x, y)));
			(x, y) )
	(* Draws the '0' leaf node. *)
	| Leaf false -> 
		(try  snd (get_node bdd_r) with
		| Not_found -> 
			draw_node (new_node bdd_r ('0', (x, y)));
			(x, y) ) 
	(* Draws branching nodes. *)
	(* Attempts to draw the node, if already previously drawn, then get its *)
	(* location and connect it to its children *)
	| Node (c, b1, b2) -> 
		let loc = 
			(try snd (get_node bdd_r) with
			| Not_found -> 
				draw_node (new_node bdd_r (c, (x, y)));
				(x, y) )
		in 
			connect_nodes loc 
				(draw_bdd b1 (((fst loc) - 50), ((snd loc) - 50)))
				(draw_bdd b2 (((fst loc) + 50), ((snd loc) - 50)));
			loc
			
(* Main function. Draws bdd and exits when Q is pressed *)
let run bdd_r = 
	Printf.printf "%s\n" (string_of_bdd bdd_r) ;
	let _ = draw_bdd bdd_r (width / 2 , height - 50) in
	loop_at_exit [Key_pressed] 
		(fun s ->
			if s.keypressed then
				match s.key with
				| 'q' -> raise Exit
				|  _ -> () 
			else 
				()
		)