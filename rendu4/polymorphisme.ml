open Expr
open Types

type schema_env = (string * schema) list

let is_typed = List.assoc

(* merges the two int list and remove the duplicates *)
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

let specialize max st =
  let instance = List.map (fun x -> (x,max ())) st.q in
  let rec replace = function
    | TVar x -> begin match List.assoc_opt x instance with
        | Some y -> TVar y
        | None -> TVar x
      end

    | TList t -> TList (replace t)
    | TRef t -> TRef (replace t)

    | TFun (t,t') -> TFun (replace t, replace t')
    | TCpl (t,t') -> TCpl (replace t, replace t')

    | t -> t
  in
  replace st.t


let rec inference_polymorphe e prob top_level =
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

  let rec add_pat_to_tenv p st env = match p, st.t with
    | Under, _ -> env
    | PConst c , _ ->
      inf_const c (specialize max st) ;
      env
    | Ident s , _ ->
      (s, st)::env
    | PCpl(p1, p2) , TCpl(t1, t2) ->
       let env' = add_pat_to_tenv p1 (make_schema t1) env in
       let env'' = add_pat_to_tenv p2 (make_schema t2) env' in
       env''
  in

  let rec inf_aux e t in_top_level vars = match e with
    | Let (p, e, e') | Rec (p, e, e') ->

      let prob' = ref (!prob) in
      inference_polymorphe e prob' (ref []) ;
      let types = Unification.unification (!prob') in
      let t_e = find_type 0 types in
      let st_e = make_schema t_e in

      let vars' = add_pat_to_tenv p st_e vars in

      inf_aux e' t in_top_level vars'


    | App(e1, e2) ->
      let m = max () in
      inf_aux e1 (TFun(TVar m, t)) in_top_level vars ;
      inf_aux e2 (TVar m) in_top_level vars

    | Fun(p, e) ->
      let t_arg = TVar (max ()) in
      let t_res = TVar (max ()) in
      let st_arg = { q = [] ; t = t_arg } in
      prob := (TFun(t_arg, t_res), t):: !prob;
      let vars' = add_pat_to_tenv p st_arg vars in
      inf_aux e t_res in_top_level vars'

    | Pattern p -> begin
        match p with
        | Under -> ()

        | PConst c -> inf_const c t

        | Ident s ->
          let sts = is_typed s vars in
          let ts = specialize max sts in
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

  in
  inf_aux e (TVar 0) true (List.map (fun (s,t) -> s, {q = [] ; t=t}) StdLib.types_stdlib)

let inference e =
  let prob = ref [] in
  let toplevel = ref [] in
  inference_polymorphe e prob toplevel ;
  !prob, !toplevel
