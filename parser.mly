%token<char> CHAR

%token TRUE FALSE
%token NOT
%token AND OR XOR ARROW DOUBLE
%token LPAREN RPAREN
%token EOF

%left DOUBLE
%left ARROW
%left OR
%left XOR
%left AND

%nonassoc NOT
%nonassoc CHAR LPAREN TRUE FALSE

%start start

%type <Bdd.formula> start
%type <Bdd.formula> expr

%%

start: 
| expr EOF { $1 }

expr: 
| CHAR { Bdd.Symbol $1 }
| TRUE { Bdd.True }
| FALSE { Bdd.False }
| NOT expr { Bdd.UOper (Bdd.Not, $2) }
| expr AND expr { Bdd.BOper ($1, Bdd.And, $3) }
| expr OR expr { Bdd.BOper ($1, Bdd.Or, $3) }
| expr XOR expr { Bdd.BOper ($1, Bdd.Xor, $3) }
| expr ARROW expr { Bdd.BOper ($1, Bdd.Implies, $3) }
| expr DOUBLE expr { Bdd.BOper ($1, Bdd.Iff, $3) }
| LPAREN expr RPAREN { $2 } 
