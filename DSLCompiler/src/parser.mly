%{
open Ast
%}

%{
(* Specify the tokens from the lexer. *)
%}
%token <string> VAR
%token PLUS MINUS TIMES EOF

%{
(* Specify the entry point to the grammar and
   the type it will produce *)
%}
%start main
%type <Ast.expr> main

%left PLUS MINUS
%left TIMES

%%
main:
  | expr EOF { $1 }

expr:
  | VAR              { Var $1 }
  | expr PLUS expr   { Binop (Plus, $1, $3) }
  | expr MINUS expr  { Binop (Minus, $1, $3) }
  | expr TIMES expr  { Binop (Times, $1, $3) }
