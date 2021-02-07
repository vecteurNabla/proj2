type token =
  | INT of (int)
  | PLUS
  | TIMES
  | MINUS
  | LPAREN
  | RPAREN
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

# 19 "parser.ml"
let yytransl_const = [|
  258 (* PLUS *);
  259 (* TIMES *);
  260 (* MINUS *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\000\000\008\000\000\000\007\000\
\000\000\000\000\000\000\000\000\001\000\003\000\000\000\005\000\
\000\000"

let yydgoto = "\002\000\
\006\000\007\000"

let yysindex = "\001\000\
\027\255\000\000\000\000\027\255\027\255\000\000\007\255\000\000\
\023\255\027\255\027\255\027\255\000\000\000\000\000\255\000\000\
\000\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\255\000\000\
\017\255"

let yygindex = "\000\000\
\000\000\252\255"

let yytablesize = 32
let yytable = "\008\000\
\009\000\001\000\011\000\000\000\000\000\015\000\016\000\017\000\
\010\000\011\000\012\000\000\000\004\000\013\000\004\000\000\000\
\004\000\004\000\006\000\000\000\006\000\000\000\006\000\006\000\
\010\000\011\000\012\000\003\000\014\000\000\000\004\000\005\000"

let yycheck = "\004\000\
\005\000\001\000\003\001\255\255\255\255\010\000\011\000\012\000\
\002\001\003\001\004\001\255\255\002\001\007\001\004\001\255\255\
\006\001\007\001\002\001\255\255\004\001\255\255\006\001\007\001\
\002\001\003\001\004\001\001\001\006\001\255\255\004\001\005\001"

let yynames_const = "\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 31 "parser.mly"
                              ( _1 )
# 93 "parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 36 "parser.mly"
                                        ( Const _1 )
# 100 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 37 "parser.mly"
                                        ( _2 )
# 107 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 38 "parser.mly"
                                        ( Add(_1,_3) )
# 115 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 39 "parser.mly"
                                        ( Mul(_1,_3) )
# 123 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 40 "parser.mly"
                                        ( Min(_1,_3) )
# 131 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 41 "parser.mly"
                                        ( Min(Const 0, _2) )
# 138 "parser.ml"
               : 'expression))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.expr)
