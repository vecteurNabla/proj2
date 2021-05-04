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

(* merges the two int list and removes the duplicates *)
let merge a b =
  let cmp x y = if x=y then 0 else if x<y then -1 else 1 in
  let a' = List.sort cmp a in
  let b' = List.sort cmp b in
  let c = List.merge cmp a' b' in
  List.sort_uniq cmp c

let rec free_tvar = function
  | TVar x -> [x]

  | TList t | TRef t ->
    free_tvar t

  | TFun (t,t') | TCpl (t,t') ->
    merge (free_tvar t) (free_tvar t')

  | _ -> []

let make_schema t =
  { q = free_tvar t ;  t = t }

let make_empty_schema t =
  { q = [] ;  t = t }


let rec find_type x = function
  | [] -> TVar x
  | (y,t)::_ when x=y -> t
  | _::next -> find_type x next
