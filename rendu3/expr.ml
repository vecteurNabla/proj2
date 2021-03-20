open Memory

type const =
    Int of int
  | Unit
  | Bool of bool
  | Nil

(* type pour les pattern *)
type pattern =
    Ident of string
  | Under
  | PConst of const
  | PCpl of pattern*pattern
  | PList_cons of pattern*pattern

(* un type pour des expressions *)
type expr =
    Const of const                  (* i *)

  | Pattern of pattern                    (* x *)
  | Let of pattern*expr*expr          (* let x = e1 in e2 *)
  | Fun of pattern*expr               (* fun x -> e *)
  | Rec of pattern*expr*expr          (* let rec x = e in e *)
  | App of expr*expr              (* e1 e2 *)

  | Cpl of expr*expr

  | Seq of expr * expr            (* e1; e2 *)
  | Aff of expr * expr            (* e1 := e2 *)
  | Der of expr                   (* !e *)

  | If of expr*expr*expr         (* if b then e1 else e2 *)

  | Match of expr * pattern_matching

  | Cons of expr*expr

  | Try of expr * pattern * expr
  | Raise of expr

and pattern_matching = (pattern*expr) list

(* type pour les valeurs *)
type value =
  | VInt of int
  | VFun of pattern*env*expr
  (* | VRec of string*pattern*env*expr *)
  | VUnit
  | VRef of address
  | VBoo of bool
  | VStdLib of (value mem -> value -> value)
  | VCpl of value*value
  | VList of value list
and env = (string*value) list

let const_to_val = function
  | Int i -> VInt i
  | Bool b -> VBoo b
  | Unit -> VUnit
  | Nil -> VList []

(* let rec val_to_expr = function
 *   | VInt x -> Const (Int x)
 *   | VUnit -> Const Unit
 *   | VBoo b -> Const (Bool b)
 *   | VList [] -> Nil
 *   | VList (h::t) -> Cons(val_to_expr h, val_to_expr (VList t))
 *   | VCpl (x,y) -> Cpl(val_to_expr x, val_to_expr y)
 *   | VFun( pattern*env*expr
 *   | VRef of address
 *   | VStdLib of (value mem -> value -> value) *)

(* cree l'expression à partir de la chaine : expr *)

let (~~) x = Pattern (Ident x)
