type token =
  | INT of (int)
  | PLUS
  | TIMES
  | MINUS
  | LPAREN
  | RPAREN
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
