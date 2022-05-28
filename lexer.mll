{
	open Parser
}
	
let char_reg_exp = ['A'-'Z']

rule token = parse 
	| [' ' '\t'] { token lexbuf }  (* Skip blanks *)
	| '(' { LPAREN }
	| ')' { RPAREN }
	| '&' { AND }
	| '|' { OR }
	| '~' { NOT }
	| "->" { ARROW }
	| "<->" { DOUBLE }
	| '^' { XOR }
	| '1' { TRUE }
	| '0' { FALSE }
	| '\n' { EOF }
	| char_reg_exp { CHAR ( Lexing.lexeme_char lexbuf 0) }
	| eof { EOF }
	| _ { Error.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0))) }
