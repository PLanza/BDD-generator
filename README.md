# BDD-generator
A Binary Decision Diagram Generator written in OCaml

## Use
Edit the `main.ml` file and write any formula using the `formula` type and convert it to a BDD using the function `bdd_of_formula`. 
Then use the `run` function to draw the BDD onto a window. 
You can find an example already written in the `main.ml` file.

To build the project make sure you can use `ocamlbuild` and have the OCaml Graphics library installed using `opam install graphics`.

Then use the command `ocamlbuild -pkg graphics main.byte` to create the binary.

Finally run the binary using `./main.byte`. 

To close the graph window properly simply press 'Q'.
