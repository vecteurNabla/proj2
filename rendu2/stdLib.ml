open Expr
open Memory

exception PrInt_not_int

let prInt env m e =
  match eval env m e with
  | VInt i -> (print_int i ; print_newline () ; VInt i )
  | _ -> raise PrInt_not_int

let ref env m e =
  let v = eval env m e in
  VRef (alloc_mem m v)

let load_stdlib env =
  ("prInt",VStdLib prInt)::
  ("ref",VStdLib ref)::
  env

