{
open Parser
}

rule token = parse
    '('     { LPAREN }
|   ')'     { RPAREN }
|   '\\'    { LAMBDA }
|   '.'     { DOT }    

|    [' ' '\t' '\n' '\r']
        { token lexbuf }

|   ['A'-'Z' 'a'-'z']*
        {VAR (Lexing.lexeme lexbuf)}
|   eof             { EOF }
