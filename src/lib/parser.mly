// parser.mly

%token                 EOF
%token <int>           LITINT
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT 
%token                 LPAREN
%token                 RPAREN
%token                 COMMA
%token                 EQ
%token                 INT
%token                 BOOL
%token                 LET
%token                 IN
%token                 IF
%token                 THEN
%token                 ELSE

%%
