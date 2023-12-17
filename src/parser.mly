%{
  open Syntax
  (* ここに書いたものは，parser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token PLUS MINUS
%token TIMES DIV
%token LET IN REC
%token AND OR
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW
%token LBRACKET RBRACKET CONS COMMA
%token MATCH WITH BAR
%token SEMISEMI

%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI               { CExp $1 }
  | decl SEMISEMI               { $1 }
;

decl:
  | LET var EQ expr                 { CDecl($2, $4) }
  | LET REC var var EQ expr         { CRecDecl($3, $4, $6) }
  | LET var EQ expr decl            { CDeclseq(CDecl($2, $4), $5) }
  | LET REC var var EQ expr decl    { CDeclseq(CRecDecl($3, $4, $6), $7)}
;

expr:
  | IF expr THEN expr ELSE expr     { EIf($2,$4,$6) }
  | LET var EQ expr IN expr         { ELet($2,$4,$6) }
  | LET REC var var EQ expr IN expr { ELetRec($3,$4,$6,$8) }
  | arith_expr EQ arith_expr        { EEq($1,$3) }
  | arith_expr LT arith_expr        { ELt($1,$3) }
  | FUN var ARROW expr              { EFun($2,$4) }
  | arith_expr                      { $1 }
  | bool_expr                       { $1 }
  | list_expr                       { $1 }
;

list_expr:
  | arith_expr CONS list_expr { ECons($1, $3) }
  | arith_expr                { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1, $3) }
  | factor_expr                 { $1 }
;

factor_expr:
  | factor_expr TIMES factor_expr { EMul($1, $3) }
  | factor_expr DIV factor_expr { EDiv($1, $3) }
  | app_expr                 { $1 }
;

app_expr:
  | app_expr atomic_expr { EApp($1, $2) }
  | atomic_expr          { $1 }
;

bool_expr:
  | bool_expr OR and_expr  { EOr($1, $3) }
  | and_expr               { $1 }
;

and_expr:
  | and_expr AND and_expr  { EAnd($1, $3) }
  | atomic_expr            { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr COMMA expr RPAR { EPair($2, $4) }
  | LBRACKET RBRACKET { ENil }
  | LPAR expr RPAR { $2 }
;

var:
  | ID     { $1 }