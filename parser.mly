%{
open Syntax
%}
%token <string>VAR
%token LAMBDA
%token DOT
%token LPAREN RPAREN
%token EOF

%left VAR
%start main
%type <Syntax.lambda_term> main
%%
main:
    exp EOF     { $1 }
;

arg_exp:
    VAR                     { Var $1 }
|   LPAREN exp RPAREN       { $2 }
|   LAMBDA VAR DOT exp      { Lambda ($2, $4) }
;    

exp:
    arg_exp         { $1 }    
|   exp arg_exp     { Apply ($1, $2) }
;
