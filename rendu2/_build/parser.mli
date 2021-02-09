type token =
  | INT of (int)
  | VAR of (Expr.var)
  | PLUS
  | TIMES
  | MINUS
  | EQUAL
  | GEQ
  | LEQ
  | GREATER
  | LOWER
  | AND
  | OR
  | NOT
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | LPAREN
  | RPAREN
  | PRINT
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
