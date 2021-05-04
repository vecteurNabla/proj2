open Expr
open Types

type schema_env = (string * schema) list

let is_typed = List.assoc

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


let rec inference_polymorphe e prob top_level vars max x =
  (* max is the highest used number for a type var, in_top_level is
   * true if we are in a top level declaration *)
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

    | PCpl(p1, p2) , TVar x ->
      let t1 = TVar (max ()) in
      let t2 = TVar (max ()) in
      prob := (TVar x, TCpl( t1, t2 )) :: !prob ;
      let env' = add_pat_to_tenv p1 (make_empty_schema t1) env in
      let env'' = add_pat_to_tenv p2  (make_empty_schema t2) env' in
      env''

    | PList (p1, p2) , TList t ->
      let env' = add_pat_to_tenv p1 (make_schema t) env in
      let env'' = add_pat_to_tenv p2 (make_schema (TList t)) env' in
      env''

    | PList (p1, p2) , TVar x ->
      let t_elem = TVar (max ()) in
      let t_list = TList t_elem in
      prob := (st.t, t_list) :: !prob ;
      let env' = add_pat_to_tenv p1 (make_empty_schema t_elem) env in
      let env'' = add_pat_to_tenv p2 (make_empty_schema t_list) env' in
      env''

    | _ -> raise Unification.Not_unifyable

  in


  let rec inf_aux e t in_top_level vars = match e with
    | Let (p, e, e') ->

      let m = max () in

      inference_polymorphe e prob (ref []) vars max m ;
      let types = Unification.unification (!prob) in

      let t_e = find_type m types in
      let st_e = ( match e with App _ -> make_empty_schema | _ -> make_schema ) t_e in

      let vars' = add_pat_to_tenv p st_e vars in

      if in_top_level && p <> Under then top_level := (p,st_e) :: !top_level ;

      inf_aux e' t in_top_level vars'

    | Rec (Ident s as p, e, e') ->

      let m = max () in

      let st_e0 = make_empty_schema (TVar m) in

      let vars0 = (s,st_e0)::vars in

      inference_polymorphe e prob (ref []) vars0 max m ;
      let types = Unification.unification (!prob) in

      let t_e = find_type m types in
      let st_e = ( match e with App _ -> make_empty_schema | _ -> make_schema ) t_e in

      let vars' = (s, st_e)::vars in

      if in_top_level && p <> Under then top_level := (p,st_e) :: !top_level ;

      inf_aux e' t in_top_level vars'

    | Rec (_,_,_) -> failwith "un nom de fonction recursive"

    | App(e1, e2) ->
      let m = max () in
      inf_aux e1 (TFun(TVar m, t)) in_top_level vars ;
      inf_aux e2 (TVar m) in_top_level vars

    | Fun(p, e) ->
      let t_arg = TVar (max ()) in
      let t_res = TVar (max ()) in
      let st_arg = make_empty_schema t_arg in
      prob := (TFun(t_arg, t_res), t):: !prob;
      let vars' = add_pat_to_tenv p st_arg vars in
      inf_aux e t_res false vars'

    | Pattern p -> begin
        match p with
        | Under -> ()

        | PConst c -> inf_const c t

        | Ident s ->
          let sts = is_typed s vars in
          let ts = specialize max sts in
          prob := (ts, t)::!prob;

        | PCpl (p1, p2) ->
          let m = max () in
          inf_aux (Pattern p1) (TVar m) in_top_level vars ;
          prob := (t, TCpl(TVar m, TVar (max ())))::!prob ;
          inf_aux (Pattern p2) (TVar (max ())) false vars

        | PList (p1, p2) ->
          let m = max () in
          prob := (t, TList(TVar m))::!prob ;
          inf_aux (Pattern p1) (TVar m) false vars ;
          inf_aux (Pattern p2) (TList (TVar m)) false vars
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

    | Match (e, l) ->
      let t_e = TVar (max ()) in
      let st_e = make_empty_schema t_e in
      inf_aux e t_e false vars;
      List.iter (fun (p, eo) ->
          let vars' = add_pat_to_tenv p st_e vars in
          inf_aux eo t false vars'
        ) l;

    | Cons(e1, e2) ->
      let m = max () in
      inf_aux e1 (TVar m) false vars;
      inf_aux e2 (TList (TVar m)) false vars;
      prob := (t, TList (TVar m))::!prob

    | Try (e, p, e') ->
      inf_aux e t in_top_level vars;
      let vars' = add_pat_to_tenv p (make_empty_schema TInt) vars in
      inf_aux e' t false vars'

    | Raise e -> inf_aux e TInt false vars

  in
  inf_aux e (TVar x) true vars

let inference e =
  let prob = ref [] in
  let toplevel = ref [] in
  let vars = (List.map (fun (s,t) -> s, make_schema t) StdLib.types_stdlib) in

  let max_ = ref 0 in
  let max () =
    incr max_ ; !max_
  in

  inference_polymorphe e prob toplevel vars max 0 ;

  let types = Unification.unification !prob in

  let t = find_type 0 types in
  let st = ( match e with App _ -> make_empty_schema | _ -> make_schema ) t in

  toplevel := !toplevel @ [(Ident "-", st)];
  types, !prob,  !toplevel
