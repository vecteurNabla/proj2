open Expr
open Types

type probleme = {
    mutable ct : (types * types) list;
    mutable cv : (string * types) list
  }

(* val inference: expr -> probleme * ((string * types) list)
 * Engendre le problème d'unification
 *)
let inference e =
  let prob = {ct = []; cv  = []} in
  let top_level = ref [] in
  (* max is the highest used number for a type var, in_top_level is
   * true if we are in a top level declaration *)
  let rec inf_aux e t max in_top_level = match e with
    | App(e1, e2) -> (
      inf_aux e1 (TFun(TVar (max + 1), t)) (max + 1) in_top_level;
      inf_aux e2 (TVar (max+1)) (max + 1) in_top_level)
    | Fun(p, e) -> (
      prob.ct <- (TFun(TVar (max+1), TVar (max+2)), t)::(prob.ct);
      inf_aux (Pattern p) (TVar (max+1)) (max+2) in_top_level;
      inf_aux e (TVar (max+2)) (max+2) in_top_level)
    | Pattern p -> (match p with
                   | Under -> ()
                   | PConst c -> (match c with
                                 | Int i -> prob.ct <- (TInt, t)::(prob.ct)
                                 | Unit -> prob.ct <- (TUnit, t)::(prob.ct)
                                 | Bool b -> prob.ct <- (TBool, t)::(prob.ct)
                                 | Nil ->
                                    prob.ct <- (TList (TVar (max+1)),t)
                                           ::(prob.ct))
                   | Ident s -> (
                     prob.cv <- (s, t)::prob.cv;
                     if in_top_level then
                       top_level := (s, t)::(!top_level))
                   | PCpl (p1, p2) -> (
                     
                  ))
  in
  inf_aux e (TVar 0) 0 true;
  prob, !top_level
