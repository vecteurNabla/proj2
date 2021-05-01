open Expr
open Types

(* type probleme = {
 *   mutable ct : (types * types) list;
 * } *)

type type_env = (string * types) list

let is_typed = List.assoc

(* let rec add_pat_to_tenv p max env = match p with
 *   | Under -> env, max
 *   | PConst c -> env, max
 *   | Ident s -> (s, TVar (max+1))::env, (max+1)
 *   | PCpl(p1, p2) -> let env', max' = add_pat_to_tenv p1 max env in
 *     add_pat_to_tenv p2 max' env'
 * 
 * 
 * (\* val inference: expr -> probleme * ((string * types) list)
 *  * Engendre le problème d'unification
 * *\)
 * let inference e =
 *   let prob = {ct = []} in
 *   let top_level = ref [] in
 *   (\* max is the highest used number for a type var, in_top_level is
 *    * true if we are in a top level declaration *\)
 *   let inf_const c t max = match c with
 *     | Int i -> prob.ct <- (TInt, t)::(prob.ct); max
 *     | Unit -> prob.ct <- (TUnit, t)::(prob.ct); max
 *     | Bool b -> prob.ct <- (TBool, t)::(prob.ct); max
 *     | Nil ->
 *       prob.ct <- (TList (TVar (max+1)),t)
 *                  ::(prob.ct); max+1
 *   in
 *   let rec inf_aux e t max in_top_level vars = match e with
 *     | App(e1, e2) ->
 *       let max' = inf_aux e1 (TFun(TVar (max + 1), t)) (max + 1) in_top_level vars in
 *       (inf_aux e2 (TVar (max'+1)) (max'+1) in_top_level vars)+1
 *     | Fun(p, e) -> (
 *         prob.ct <- (TFun(TVar (max+1), TVar (max+2)), t)::(prob.ct);
 *         let vars', max' = add_pat_to_tenv p max vars in
 *         inf_aux e (TVar (max')) (max') in_top_level vars')
 *     | Pattern p -> (match p with
 *         | Under -> max
 *         | PConst c -> inf_const c t max
 *         | Ident s -> (
 *             let ts = is_typed s vars in
 *             prob.ct <- (ts, t)::(prob.ct);
 *             if in_top_level then
 *               top_level := (s, ts)::(!top_level)); max
 *         | PCpl (p1, p2) -> (
 *             let max' = inf_aux (Pattern p1) (TVar (max+1)) (max+2) in_top_level vars in
 *             prob.ct <- (t, TCpl(TVar (max+1), TVar (max'+1)))::prob.ct;
 *             inf_aux (Pattern p2) (TVar (max'+1)) (max'+1) in_top_level vars)
 *         | PList (p1, p2) -> (
 *             prob.ct <- (t, TList(TVar (max+1)))::prob.ct;
 *             let max' = inf_aux (Pattern p1) (TVar (max+1)) (max+1) in_top_level vars in
 *             inf_aux (Pattern p2) (TList (TVar (max+1))) (max') in_top_level vars))
 *     | Let (p, e, e') | Rec (p, e, e') -> (
 *         let max' = inf_aux e (TVar (max+1)) (max+1) false vars in
 *         let vars', max'' = add_pat_to_tenv p max' vars in
 *         inf_aux e' t max'' in_top_level vars')
 *     | Val v -> (match v with
 *         | Const c -> inf_const c t max
 *         | _ -> failwith "this value cannot be typed yet\n"
 *       )
 *     | Cpl (e1, e2) -> (
 *         let max' = inf_aux e1 (TVar (max+1)) (max+1) false vars in
 *         prob.ct <- (t, TCpl(TVar (max+1), TVar(max'+1)))::prob.ct;
 *         inf_aux e2 (TVar (max'+1)) (max'+1) false vars
 *       )
 *     | Aff (e1, e2) -> (
 *         prob.ct <- (t, TUnit)::prob.ct;
 *         let max' = (inf_aux e2 (TVar (max+1)) (max+1) false vars) in
 *         inf_aux e1 (TRef (TVar (max+1))) (max') false vars
 *       )
 *     | Der e -> (
 *         prob.ct <- (t, TVar (max+1))::prob.ct;
 *         inf_aux e (TVar (max+1)) (max+1) false vars
 *       )
 *     | If (b, et, ef) -> (
 *         prob.ct <- (t, TBool)::prob.ct;
 *         let max' = inf_aux b (TBool) max false vars in
 *         let max'' = inf_aux et (TVar (max'+1)) (max'+1) false vars in
 *         inf_aux ef (TVar (max'+1)) (max''+1) false vars
 *       )
 *   in
 *   inf_aux e (TVar 0) 0 true [];
 *   prob, !top_level *)


(******************)

let inference e =
  let prob = ref [] in
  let top_level = ref [] in
  (* max is the highest used number for a type var, in_top_level is
   * true if we are in a top level declaration *)
  let max_ = ref 0 in
  let max () =
    incr max_ ; !max_
  in

  let inf_const c t = match c with
    | Int i -> prob := (TInt, t):: !prob
    | Unit -> prob := (TUnit, t):: !prob
    | Bool b -> prob := (TBool, t):: !prob
    | Nil -> prob := (TList (TVar (max ())),t):: !prob
  in

  let rec add_pat_to_tenv p env = match p with
    | Under -> env, TVar (max ())
    | PConst c -> env, (match c with
                       | Int i -> TInt
                       | Unit -> TUnit
                       | Bool b -> TBool
                       | Nil -> TVar (max ()))
    | Ident s ->
       let m = max () in
       (s, TVar m)::env, (TVar m)
    | PCpl(p1, p2) ->
       let env', t1 = add_pat_to_tenv p1 env in
       let env'', t2 = add_pat_to_tenv p2 env' in
       env'', TCpl (t1, t2)
    | PList (p1, p2) ->
       let env', t1 = add_pat_to_tenv p1 env in
       let env'', t2 = add_pat_to_tenv p2 env' in
       prob := (t2, TList t1)::!prob;
       env'', t2
  in
  let rec inf_aux e t in_top_level vars = match e with
    | App(e1, e2) ->
      let m = max () in
      inf_aux e1 (TFun(TVar m, t)) in_top_level vars ;
      inf_aux e2 (TVar m) in_top_level vars

    | Fun(p, e) ->    (* /!\ il faut typer les patterns ! *)
      let m = max () in
      let m' = max () in
      prob := (TFun(TVar m, TVar m'), t):: !prob;
      let vars', tp = add_pat_to_tenv p vars in
      prob := (tp, TVar m)::!prob;
      inf_aux e (TVar m') in_top_level vars'

    | Pattern p -> begin
        match p with
        | Under -> ()

        | PConst c -> inf_const c t

        | Ident s ->
          let ts = is_typed s vars in
          prob := (ts, t)::!prob;
          if in_top_level then
            top_level := (s, ts)::(!top_level)

        | PCpl (p1, p2) ->
          let m = max () in
          inf_aux (Pattern p1) (TVar m) in_top_level vars ;
          prob := (t, TCpl(TVar m, TVar (max ())))::!prob ;
          inf_aux (Pattern p2) (TVar (max ())) in_top_level vars

        | PList (p1, p2) ->
          let m = max () in
          prob := (t, TList(TVar m))::!prob ;
          inf_aux (Pattern p1) (TVar m) in_top_level vars ;
          inf_aux (Pattern p2) (TList (TVar m)) in_top_level vars
      end

    | Let (p, e, e') | Rec (p, e, e') ->   (* /!\ il faut typer les patterns ! *)
       let m = max () in
       inf_aux e (TVar m) false vars ;
       let vars', tp = add_pat_to_tenv p vars in
       prob := (tp, TVar m)::!prob;
       inf_aux e' t in_top_level vars'

    | Val v -> begin match v with
        | Const c -> inf_const c t
        | _ -> failwith "this value cannot be typed yet\n"
      end

    | Cpl (e1, e2) ->
      let m = max () in
      let m' = max () in
      prob := (t, TCpl(TVar m, TVar m'))::!prob;
      inf_aux e1 (TVar m) false vars ;
      inf_aux e2 (TVar m') false vars

    | Aff (e1, e2) ->
      prob := (t, TUnit)::!prob;
      let m = max () in
      inf_aux e2 (TVar m) false vars ;
      inf_aux e1 (TRef (TVar m)) false vars

    | Der e ->
      let m = max () in
      prob := (t, TVar m)::!prob;
      inf_aux e (TRef (TVar m)) false vars

    | If (b, et, ef) ->
      let m = max (); in
      prob := (t, TVar m)::!prob;
      inf_aux b (TBool) false vars;
      inf_aux et (TVar m) false vars;
      inf_aux ef (TVar m) false vars

    | Match (e, l) ->
       let m_in = max () in
       (* let m_out = max () in
        * prob := (t, TVar m_out)::!prob; *)
       inf_aux e (TVar m_in) false vars;
       List.iter (fun (p, eo) ->
           let vars', tp = add_pat_to_tenv p vars in
           prob := (tp, TVar m_in)::!prob;
           (* inf_aux eo (TVar m_out) false vars' *)
           inf_aux eo t false vars'
         ) l;
    | Cons(e1, e2) ->
       let m = max () in
       inf_aux e1 (TVar m) false vars;
       inf_aux e2 (TList (TVar m)) false vars;
       prob := (t, TList (TVar m))::!prob

    | Try (e, p, e') ->
       inf_aux e (TVar (max ())) in_top_level vars;
       let vars', tp = add_pat_to_tenv p vars in
       inf_aux e' t false vars'

    | Raise e ->
      ()
  in
  inf_aux e (TVar 0) true StdLib.types_stdlib;
  !prob, !top_level
