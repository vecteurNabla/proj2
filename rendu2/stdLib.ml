open Expr
open Memory

exception PrInt_not_int

let _prInt m v =
  match v with
  | VInt i -> (print_int i ; print_newline () ; VInt i )
  | _ -> raise PrInt_not_int

let _ref m v =
  VRef (alloc_mem m v)

let _not m v =
  match v with
  | VBoo b -> VBoo (not b)
  | _ -> raise (Not_expected "un booleen")

let load_stdlib env =
  ("prInt",VStdLib _prInt)::
  ("ref",VStdLib _ref)::
  ("not",VStdLib _not)::
  env
