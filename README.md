# BDD-generator
A Binary Decision Diagram Generator written in OCaml

## How to Use
Write your own logical expression into a file where the operators are represented with the following characters:
- NOT : `~`
- AND : `&`
- OR : `|`
- XOR : `^`
- IMPLIES :  `->`
- IF-AND-ONLY-IF : `<->` 

Then edit the `main.ml` file to use the file containing your expression, or simply edit the example.bdd file to contain your expression .

To build the project make sure you can use `ocamlbuild` and have the OCaml Graphics library installed using `opam install graphics`.

Then use the command `ocamlbuild -pkg graphics main.byte` to create the binary.

Finally run the binary using `./main.byte`. 

To close the graph window properly simply press 'Q'.
