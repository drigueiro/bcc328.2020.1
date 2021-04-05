// parser.mly

%token                 EOF
%token <int>           LITINT
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT
%token                 EQ
%token                 COMMA
%token                 LPAREN
%token                 RPAREN
%token                 INT
%token                 BOOL
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LET
%token                 IN

%start <Absyn.lfundecs> program

%nonassoc IN ELSE
%nonassoc LT
%left PLUS

%%

program:
| x=nonempty_list(fundec) EOF { $loc , x }

exps: 
| x=separated_nonempty_list(COMMA, exp) { x }

exp:
| x=LITINT                       { $loc , Absyn.IntExp x }
| x=ID                           { $loc , Absyn.IdExp x }
| x=exp op=operator y=exp        { $loc , Absyn.OpExp (op, x, y) }
| IF x=exp THEN y=exp ELSE z=exp { $loc , Absyn.ConditionalExp (x, y, z)}
| x=ID LPAREN y=exps RPAREN      { $loc , Absyn.FunctionExp (x, y) }
| LET x=ID EQ y=exp IN z=exp     { $loc , Absyn.DeclarationExp (x, y, z) }

%inline operator:
| PLUS { Absyn.Plus }
| LT   { Absyn.LT }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }

typeid:
| INT x=ID   { (Absyn.Int, x) }
| BOOL x= ID { (Absyn.Bool, x) }

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }
