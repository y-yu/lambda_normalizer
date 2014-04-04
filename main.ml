let parse str = Parser.main Lexer.token (Lexing.from_string str)

let normalizer str = Lambda.normalizer (parse str)
