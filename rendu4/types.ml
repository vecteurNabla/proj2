type types =
  | TUnit
  | TInt
  | TBool
  | TExn
  | TList of types
  | TRef of types
  | TFun of types*types
  | TCpl of types*types

  | TVar of int

type schema = { q : int list ; t : types }

let rec find_type x = function
  | [] -> TVar x
  | (y,t)::_ when x=y -> t
  | _::next -> find_type x next
