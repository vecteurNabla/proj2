(* type pour les constantes *)
type const =
    Int of int
  | Unit
  | Bool of bool
  | Nil

(* type pour les valeurs *)
type value =
    Const of const
  | Ref of Memory.address
  | StdLib of (value Memory.mem -> value -> expr)
  | VFun of pattern * env * expr
  | VCpl of value * value
  | VList of value * value

and env = (string*value) list

(* type pour les pattern *)
and pattern =
    Ident of string
  | Under
  | PConst of const
  | PCpl of pattern*pattern
  | PList of pattern*pattern

(* un type pour des expressions *)
and expr =
    Val of value                 (* i *)

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

(* cree une expression a partir ... *)
(* ... d'une constante *)
let (~&) c = Val (Const c)

(* d'un identifiant (sous forme de string) *)
let (~~) x = Pattern (Ident x)
