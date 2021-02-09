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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    (* --- préambule: ici du code Caml --- *)

    open Expr   (* rappel: dans expr.ml: 
                type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

         
# 37 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* TIMES *);
  261 (* MINUS *);
  262 (* EQUAL *);
  263 (* GEQ *);
  264 (* LEQ *);
  265 (* GREATER *);
  266 (* LOWER *);
  267 (* AND *);
  268 (* OR *);
  269 (* NOT *);
  270 (* TRUE *);
  271 (* FALSE *);
  272 (* IF *);
  273 (* THEN *);
  274 (* ELSE *);
  275 (* LET *);
  276 (* IN *);
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* PRINT *);
  280 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\003\000\003\000\002\000\001\000\
\006\000\006\000\002\000\003\000\003\000\003\000\002\000\003\000\
\003\000\003\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\008\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\007\000\000\000\020\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\000\000\005\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\010\000\017\000\018\000"

let yysindex = "\023\000\
\061\255\000\000\000\000\000\000\061\255\038\255\023\255\061\255\
\061\255\000\000\008\255\000\000\038\255\000\000\000\000\038\255\
\187\255\080\255\036\255\254\254\106\255\061\255\061\255\061\255\
\000\000\044\255\118\255\026\255\061\255\061\255\061\255\061\255\
\038\255\038\255\061\255\061\255\000\000\040\255\000\000\040\255\
\000\000\106\255\106\255\106\255\106\255\044\255\044\255\042\255\
\013\255\061\255\061\255\106\255\106\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\124\255\000\000\000\000\000\000\
\000\000\244\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\078\255\000\000\096\255\
\000\000\053\255\056\255\164\255\176\255\253\254\019\255\000\000\
\000\000\000\000\000\000\142\255\160\255"

let yygindex = "\000\000\
\000\000\255\255\249\255"

let yytablesize = 198
let yytable = "\011\000\
\022\000\023\000\024\000\012\000\015\000\026\000\020\000\021\000\
\028\000\015\000\022\000\023\000\024\000\013\000\027\000\022\000\
\023\000\024\000\013\000\037\000\038\000\039\000\040\000\001\000\
\019\000\046\000\047\000\042\000\043\000\044\000\045\000\025\000\
\051\000\048\000\049\000\014\000\033\000\034\000\003\000\004\000\
\014\000\036\000\005\000\023\000\022\000\023\000\024\000\041\000\
\052\000\053\000\013\000\014\000\015\000\006\000\033\000\034\000\
\007\000\000\000\016\000\050\000\009\000\003\000\004\000\017\000\
\017\000\005\000\016\000\016\000\000\000\017\000\000\000\000\000\
\016\000\000\000\017\000\000\000\006\000\016\000\000\000\007\000\
\004\000\008\000\004\000\009\000\004\000\004\000\004\000\004\000\
\004\000\004\000\033\000\034\000\000\000\000\000\004\000\004\000\
\035\000\004\000\006\000\004\000\006\000\004\000\006\000\006\000\
\006\000\006\000\006\000\006\000\022\000\023\000\024\000\000\000\
\006\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\
\022\000\023\000\024\000\000\000\029\000\030\000\031\000\032\000\
\000\000\000\000\011\000\011\000\011\000\011\000\011\000\011\000\
\000\000\000\000\000\000\037\000\011\000\011\000\000\000\011\000\
\000\000\011\000\000\000\011\000\010\000\010\000\010\000\010\000\
\010\000\010\000\000\000\000\000\000\000\000\000\010\000\010\000\
\000\000\010\000\000\000\010\000\000\000\010\000\009\000\009\000\
\009\000\009\000\009\000\009\000\000\000\000\000\019\000\019\000\
\009\000\009\000\000\000\009\000\019\000\009\000\000\000\009\000\
\000\000\019\000\018\000\018\000\000\000\022\000\023\000\024\000\
\018\000\029\000\030\000\031\000\032\000\018\000"

let yycheck = "\001\000\
\003\001\004\001\005\001\005\000\017\001\013\000\008\000\009\000\
\016\000\022\001\003\001\004\001\005\001\017\001\016\000\003\001\
\004\001\005\001\022\001\022\001\022\000\023\000\024\000\001\000\
\002\001\033\000\034\000\029\000\030\000\031\000\032\000\024\001\
\020\001\035\000\036\000\017\001\011\001\012\001\001\001\002\001\
\022\001\006\001\005\001\004\001\003\001\004\001\005\001\022\001\
\050\000\051\000\013\001\014\001\015\001\016\001\011\001\012\001\
\019\001\255\255\021\001\018\001\023\001\001\001\002\001\011\001\
\012\001\005\001\011\001\012\001\255\255\017\001\255\255\255\255\
\017\001\255\255\022\001\255\255\016\001\022\001\255\255\019\001\
\003\001\021\001\005\001\023\001\007\001\008\001\009\001\010\001\
\011\001\012\001\011\001\012\001\255\255\255\255\017\001\018\001\
\017\001\020\001\003\001\022\001\005\001\024\001\007\001\008\001\
\009\001\010\001\011\001\012\001\003\001\004\001\005\001\255\255\
\017\001\018\001\255\255\020\001\255\255\022\001\255\255\024\001\
\003\001\004\001\005\001\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\007\001\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\022\001\017\001\018\001\255\255\020\001\
\255\255\022\001\255\255\024\001\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\017\001\018\001\
\255\255\020\001\255\255\022\001\255\255\024\001\007\001\008\001\
\009\001\010\001\011\001\012\001\255\255\255\255\011\001\012\001\
\017\001\018\001\255\255\020\001\017\001\022\001\255\255\024\001\
\255\255\022\001\011\001\012\001\255\255\003\001\004\001\005\001\
\017\001\007\001\008\001\009\001\010\001\022\001"

let yynames_const = "\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  EQUAL\000\
  GEQ\000\
  LEQ\000\
  GREATER\000\
  LOWER\000\
  AND\000\
  OR\000\
  NOT\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  IN\000\
  LPAREN\000\
  RPAREN\000\
  PRINT\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 37 "parser.mly"
                              ( _1 )
# 203 "parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                                                         ( Const _1 )
# 210 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 43 "parser.mly"
                                                         ( _2 )
# 217 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 44 "parser.mly"
                                                         ( Add(_1,_3) )
# 225 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 45 "parser.mly"
                                                         ( Mul(_1,_3) )
# 233 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 46 "parser.mly"
                                                         ( Min(_1,_3) )
# 241 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 47 "parser.mly"
                                                         ( Min(Const 0, _2) )
# 248 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Expr.var) in
    Obj.repr(
# 48 "parser.mly"
                                                         ( Var _1 )
# 255 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Expr.var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 49 "parser.mly"
                                                         ( Let(_2, _4, _6) )
# 264 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'bool_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 50 "parser.mly"
                                                         ( If(_2, _4, _6) )
# 273 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 51 "parser.mly"
                                                         ( Print(_2) )
# 280 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expression) in
    Obj.repr(
# 55 "parser.mly"
                                             ( _2 )
# 287 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expression) in
    Obj.repr(
# 56 "parser.mly"
                                             ( And(_1 ,_3) )
# 295 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expression) in
    Obj.repr(
# 57 "parser.mly"
                                             ( Or(_1 ,_3) )
# 303 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expression) in
    Obj.repr(
# 58 "parser.mly"
                                             ( Not _2 )
# 310 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 59 "parser.mly"
                                             ( Leq(_1, _3) )
# 318 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 60 "parser.mly"
                                             ( Geq(_1, _3) )
# 326 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 61 "parser.mly"
                                             ( Lt(_1, _3) )
# 334 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 62 "parser.mly"
                                             ( Gt(_1, _3) )
# 342 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                             ( True )
# 348 "parser.ml"
               : 'bool_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                             ( False )
# 354 "parser.ml"
               : 'bool_expression))
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
