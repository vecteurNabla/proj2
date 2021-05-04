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


type quantifiers = (int, int) Hashtbl.t
(* key: num de var, data: num de l'instanciation (n'est utilise que dans specialize) *)

type schema = { q : quantifiers ; t : types }

(** on cree le schema associe au type en parametre :
 * on calcule les variables libres à quantifier
 * filter permet de choisir les variables acceptables, par defaut tout est accepte *)
let make_schema ?(filter = fun _ -> true) t =
  let free_tvar = Hashtbl.create 0 in
  let rec fill_tbl = function
    | TVar x -> if not (Hashtbl.mem free_tvar x) && filter x then Hashtbl.add free_tvar x 0

    | TList t | TRef t ->
      fill_tbl t

    | TFun (t,t') | TCpl (t,t') ->
      fill_tbl t ; fill_tbl t'

    | _ -> ()
  in
  fill_tbl t ;
  { q = free_tvar ;  t = t }

(** cree un schema sans quantification
 * equivaut a `make_schema ~filter:(fun _ -> false)`
 * est plus efficace que cette derniere *)
let make_empty_schema t =
  { q = Hashtbl.create 0 ;  t = t }


(** trouve un type dans une liste de (int*types) *)
let rec find_type x = function
  | [] -> TVar x
  | (y,t)::_ when x=y -> t
  | _::next -> find_type x next
