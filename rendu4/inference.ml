open Expr
open Types

type probleme = {
    mutable ct : (types * types) list;
  }

type type_env = (string * types) list

let is_typed = List.assoc

let rec add_pat_to_tenv p max env = match p with
  | Under -> env, max
  | PConst c -> env, max
  | Ident s -> (s, TVar (max+1))::env, (max+1)
  | PCpl(p1, p2) -> let env', max' = add_pat_to_tenv p1 max env in
                   add_pat_to_tenv p2 max' env'
             

(* val inference: expr -> probleme * ((string * types) list)
 * Engendre le problème d'unification
 *)
let inference e =
  let prob = {ct = []} in
  let top_level = ref [] in
  (* max is the highest used number for a type var, in_top_level is
   * true if we are in a top level declaration *)
  let rec inf_aux e t max in_top_level vars = match e with
    | App(e1, e2) -> (
      inf_aux e1 (TFun(TVar (max + 1), t)) (max + 1) in_top_level vars;
      inf_aux e2 (TVar (max+1)) (max + 1) in_top_level vars)
    | Fun(p, e) -> (
      prob.ct <- (TFun(TVar (max+1), TVar (max+2)), t)::(prob.ct);
      let vars', max' = add_pat_to_tenv p max vars in
      inf_aux e (TVar (max')) (max') in_top_level vars')
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
                     let ts = is_typed s vars in
                     if in_top_level then
                       top_level := (s, ts)::(!top_level))
                   | PCpl (p1, p2) -> (
                     
                  ))
  in
  inf_aux e (TVar 0) 0 true;
  prob, !top_level
