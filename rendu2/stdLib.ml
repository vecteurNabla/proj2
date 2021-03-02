open Expr
open Memory

exception PrInt_not_int

let _prInt m = function
  | VInt i -> (print_int i ; print_newline () ; VInt i )
  | _ -> raise PrInt_not_int

let _ref m v =
  VRef (alloc_mem m v)



let _add1 i m = function
  | VInt j -> VInt (i+j)
  | _ -> raise (Not_expected "un entier")
let _add m = function
  | VInt i -> VStdLib (_add1 i)
  | _ -> raise (Not_expected "un entier")

let _mul1 i m = function
  | VInt j -> VInt (i*j)
  | _ -> raise (Not_expected "un entier")
let _mul m = function
  | VInt i -> VStdLib (_mul1 i)
  | _ -> raise (Not_expected "un entier")

let _min1 i m = function
  | VInt j -> VInt (i-j)
  | _ -> raise (Not_expected "un entier")
let _min m = function
  | VInt i -> VStdLib (_min1 i)
  | _ -> raise (Not_expected "un entier")

let _div1 i m = function
  | VInt 0 -> raise Div_by_Zero
  | VInt j -> VInt (i/j)
  | _ -> raise (Not_expected "un entier")
let _div m = function
  | VInt i -> VStdLib (_div1 i)
  | _ -> raise (Not_expected "un entier")


let _and1 i m = function
  | VBoo j -> VBoo (i&&j)
  | _ -> raise (Not_expected "un booléen")
let _and m = function
  | VBoo i -> VStdLib (_and1 i)
  | _ -> raise (Not_expected "un booléen")

let _or1 i m = function
  | VBoo j -> VBoo (i||j)
  | _ -> raise (Not_expected "un booléen")
let _or m = function
  | VBoo i -> VStdLib (_and1 i)
  | _ -> raise (Not_expected "un booléen")

let _not m = function
  | VBoo b -> VBoo (not b)
  | _ -> raise (Not_expected "un booleen")


let _leq1 i m = function
  | VInt j -> VBoo (i<=j)
  | _ -> raise (Not_expected "un entier")
let _leq m = function
  | VInt i -> VStdLib (_leq1 i)
  | _ -> raise (Not_expected "un entier")

let _geq1 i m = function
  | VInt j -> VBoo (i>=j)
  | _ -> raise (Not_expected "un entier")
let _geq m = function
  | VInt i -> VStdLib (_geq1 i)
  | _ -> raise (Not_expected "un entier")

let _lt1 i m = function
  | VInt j -> VBoo (i<j)
  | _ -> raise (Not_expected "un entier")
let _lt m = function
  | VInt i -> VStdLib (_lt1 i)
  | _ -> raise (Not_expected "un entier")

let _gt1 i m = function
  | VInt j -> VBoo (i>j)
  | _ -> raise (Not_expected "un entier")
let _gt m = function
  | VInt i -> VStdLib (_gt1 i)
  | _ -> raise (Not_expected "un entier")

let _eq1 i m = function
  | VInt j -> VBoo (i=j)
  | _ -> raise (Not_expected "un entier")
let _eq m = function
  | VInt i -> VStdLib (_eq1 i)
  | _ -> raise (Not_expected "un entier")

let _neq1 i m = function
  | VInt j -> VBoo (i<>j)
  | _ -> raise (Not_expected "un entier")
let _neq m = function
  | VInt i -> VStdLib (_neq1 i)
  | _ -> raise (Not_expected "un entier")


let load_stdlib env =
  ("(+)",VStdLib _add)::
  ("(*)",VStdLib _mul)::
  ("(-)",VStdLib _min)::
  ("(/)",VStdLib _div)::

  ("(&&)", VStdLib _and)::
  ("(||)", VStdLib _or)::
  ("not",VStdLib _not)::

  ("(<=)", VStdLib _leq)::
  ("(>=)", VStdLib _geq)::
  ("(<)", VStdLib _lt)::
  ("(>)", VStdLib _gt)::
  ("(=)", VStdLib _eq)::
  ("(<>)", VStdLib _neq)::

  ("prInt",VStdLib _prInt)::
  ("ref",VStdLib _ref)::
  env
