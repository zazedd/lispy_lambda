%token <int> INT
%token DEF
%token DEFN
%token FN
%token EOF
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token <Ast.variable> IDENT

%start <Ast.Parsed.t> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | LPAREN; e = expr; l = list (expr); RPAREN { App (e, l) }
  | LPAREN; DEF; name = IDENT; e = expr; RPAREN { Def (name, e) }
  | LPAREN; DEFN; name = IDENT; LBRACK; l = list (IDENT); RBRACK; e = expr; RPAREN { Defn (name, l, e) }
  | LPAREN; FN; LBRACK; l = list (IDENT); RBRACK; e = expr; RPAREN { Fn (l, e) }
  | i = INT { Const (i) }
  | i = IDENT { Var (i) }
;



