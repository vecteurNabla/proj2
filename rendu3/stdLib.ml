open Expr
open Eval
open Memory

exception PrInt_not_int

(* fonctions std simples, ie sans application partielles *)
let _prInt m = function
  | Const (Int i) -> (print_int i ; print_newline () ; ~& (Int i) )
  | _ -> raise PrInt_not_int

let _ref m v =
  Val (Ref (alloc_mem m v))

let _not m = function
  | Const (Bool b) -> ~& (Bool (not b))
  | _ -> raise (Not_expected "un booleen")

let _fst m v = Val (!& v)

let _snd m v = Val (!&& v)

let simples =
  ("fst", StdLib _fst)::
  ("snd", StdLib _snd)::

  ("not", StdLib _not)::

  ("prInt", StdLib _prInt)::
  ("ref", StdLib _ref)::
  []


(* fonctions doubles, ie avec application partielle *)

(* fonctions non traduites *)
let arithop1 op i m = function
  | Const (Int j) -> ~& (Int (op i j))
  | _ -> raise (Not_expected "un entier")
let arithop op m = function
  | Const (Int i) -> Val (StdLib (arithop1 op i))
  | _ -> raise (Not_expected "un entier")


let boolop1 op i m = function
  | Const (Bool j) -> ~& (Bool (op i j))
  | v -> raise (Not_expected "un booléen")
let boolop op m = function
  | Const (Bool i) -> Val (StdLib (boolop1 op i))
  | v -> raise (Not_expected "un booléen")


let cmp1 op i m = function
  | Const (Int j) -> ~& (Bool (op i j))
  | _ -> raise (Not_expected "un entier")
let cmp op m = function
  | Const (Int i) -> Val (StdLib (cmp1 op i))
  | _ -> raise (Not_expected "un entier")

let _eq1 v m v' = ~& (Bool (v = v'))
let _eq m v = Val (StdLib (_eq1 v))

let _neq1 v m v' = ~& (Bool (v <> v'))
let _neq m v = Val (StdLib (_neq1 v))

let doubles =
  ("(+)", StdLib (arithop ( + )))::
  ("(*)", StdLib (arithop ( * )))::
  ("(-)", StdLib (arithop ( - )))::
  ("(/)", StdLib (arithop ( / )))::

  ("(&&)", StdLib (boolop ( && )))::
  ("(||)", StdLib (boolop ( || )))::

  ("(<=)", StdLib (cmp ( <= )))::
  ("(>=)", StdLib (cmp ( >= )))::
  ("(<)", StdLib (cmp ( < )))::
  ("(>)", StdLib (cmp ( > )))::
  ("(=)", StdLib _eq)::
  ("(<>)", StdLib _neq)::
  []

(* fonctions traduites *)
let t = Transformation.transform_stdlib

let t_arithop op = t begin
    fun m -> function
  | Const (Int i) -> Val (StdLib (t (arithop1 op i)))
  | _ -> raise (Not_expected "un entier")
end

let t_boolop op = t begin
    fun m -> function
  | Const (Bool i) -> Val (StdLib (t (boolop1 op i)))
  | _ -> raise (Not_expected "un booléen")
end

let t_cmp op = t begin
    fun m -> function
  | Const (Int i) -> Val (StdLib (t (cmp1 op i)))
  | _ -> raise (Not_expected "un entier")
end

let _t_eq = t begin
    fun m v -> Val (StdLib (t (_eq1 v)))
end

let _t_neq = t begin
    fun m v -> Val (StdLib ( t(_neq1 v)))
end

let t_doubles =
  ("(+)", StdLib (t_arithop ( + )))::
  ("(*)", StdLib (t_arithop ( * )))::
  ("(-)", StdLib (t_arithop ( - )))::
  ("(/)", StdLib (t_arithop ( / )))::

  ("(&&)", StdLib (t_boolop ( && )))::
  ("(||)", StdLib (t_boolop ( || )))::

  ("(<=)", StdLib (t_cmp ( <= )))::
  ("(>=)", StdLib (t_cmp ( >= )))::
  ("(<)", StdLib (t_cmp ( < )))::
  ("(>)", StdLib (t_cmp ( > )))::
  ("(=)", StdLib _t_eq)::
  ("(<>)", StdLib _t_neq)::
  []

let stdlib =
 simples @ doubles

let rec transform = function
  | [] -> []
  | (name, StdLib f)::t -> (name, StdLib (Transformation.transform_stdlib f)) :: transform t
  | h::t -> h::transform t

 let stdlib_transform =
   (transform simples) @ t_doubles
