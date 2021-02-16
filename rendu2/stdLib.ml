open Expr
open Memory

exception PrInt_not_int

let prInt m v =
  match v with
  | VInt i -> (print_int i ; print_newline () ; VInt i )
  | _ -> raise PrInt_not_int

let ref m v =
  VRef (alloc_mem m v)

let load_stdlib env =
  ("prInt",VStdLib prInt)::
  ("ref",VStdLib ref)::
  env

