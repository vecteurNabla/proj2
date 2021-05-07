open Expr
open Types

type schema_env = (string * schema) list

let is_typed = List.assoc

(** specialisation d'un schema :
 * on instancie les variables liees puis on les remplace *)
let specialize max st =
  let instanciate x _  = Some (max ()) in
  Hashtbl.filter_map_inplace instanciate st.q ;

  let rec replace = function
    | TVar x -> begin match Hashtbl.find_opt st.q x with
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

(** generalisation d'un type :
 * on trouve les varaibles presentes dans l'env
 * puis on construit le schema en ne quantifiant pas sur les variables qui y sont
 *)
let generalize env t =
  let vars = Hashtbl.create 0 in
  let rec get_vars = function
    | TVar x -> Hashtbl.add vars x ()
    | TList t | TRef t ->
      get_vars t
    | TFun (t,t') | TCpl (t,t') ->
      get_vars t ; get_vars t'
    | _ -> ()
  in
  List.iter (fun (_,st) -> get_vars st.t) env ;

  let filter x = not (Hashtbl.mem vars x) in

  let st = make_schema ~filter:filter t in
  st


(** actualise les variables libres de l'environnement avec les types trouves lors de l'unifictaion *)
let update_schema types st =
  let rec update q = function
    | TVar x as t ->
      if Hashtbl.mem q x then t
      else find_type x types

    | TList t -> TList (update q t)
    | TRef t -> TRef (update q t)

    | TFun (t,t') -> TFun (update q t, update q t')
    | TCpl (t,t') -> TCpl (update q t, update q t')

    | t -> t
  in
  {q = st.q ; t = update st.q st.t}

let update_env types =
  List.map (fun (s,st) -> (s, update_schema types st))


(** fonction principale qui effectue l'inference *)
let rec inference_polymorphe e prob top_level env max x =
  (* inference simple : pour une constante *)
  let inf_const c t = match c with
    | Int i -> prob := (TInt, t):: !prob
    | Unit -> prob := (TUnit, t):: !prob
    | Bool b -> prob := (TBool, t):: !prob
    | Nil -> prob := (TList (TVar (max ())),t):: !prob
  in

  (* ajoute à l'environnemnt le pattern de schema de type donne *)
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

  (* moulinette *)
  let rec inf_aux e t in_top_level env = match e with
    | Let (p, e, e') ->

      let m = max () in

      inference_polymorphe e prob (ref []) env max m ;
      let types = Unification.unification (!prob) in

      let t_e = find_type m types in
      let env' = update_env types env in

      let st_e = ( match e,t_e with _ , TRef _ | App _ , _ -> make_empty_schema | _ -> generalize env' ) t_e in

      let env'' = add_pat_to_tenv p st_e env' in

      if in_top_level && p <> Under then top_level := (p,st_e) :: !top_level ;

      inf_aux e' t in_top_level env''


    | Rec (Ident s as p, e, e') ->

      let m = max () in

      let st_e0 = make_empty_schema (TVar m) in

      let env0 = (s,st_e0)::env in

      inference_polymorphe e prob (ref []) env0 max m ;
      let types = Unification.unification (!prob) in

      let t_e = find_type m types in
      let env' = update_env types env in

      let st_e = ( match e,t_e with _ , TRef _ | App _ , _ -> make_empty_schema | _ -> generalize env' ) t_e in

      let env'' = add_pat_to_tenv p st_e env' in

      if in_top_level && p <> Under then top_level := (p,st_e) :: !top_level ;

      inf_aux e' t in_top_level env''


    | Rec (_,_,_) -> failwith "un nom de fonction recursive"


    | App(e1, e2) ->
      let m = max () in
      inf_aux e1 (TFun(TVar m, t)) in_top_level env ;
      inf_aux e2 (TVar m) in_top_level env


    | Fun(p, e) ->
      let t_arg = TVar (max ()) in
      let t_res = TVar (max ()) in
      let st_arg = make_empty_schema t_arg in
      prob := (TFun(t_arg, t_res), t):: !prob;
      let env' = add_pat_to_tenv p st_arg env in
      inf_aux e t_res false env'


    | Pattern p -> begin
        match p with
        | Under -> ()

        | PConst c -> inf_const c t

        | Ident s ->
          let st_s = is_typed s env in
          let t_s = specialize max st_s in
          prob := (t_s, t)::!prob;

        | PCpl (p1, p2) ->
          let m = max () in
          inf_aux (Pattern p1) (TVar m) in_top_level env ;
          prob := (t, TCpl(TVar m, TVar (max ())))::!prob ;
          inf_aux (Pattern p2) (TVar (max ())) false env

        | PList (p1, p2) ->
          let m = max () in
          prob := (t, TList(TVar m))::!prob ;
          inf_aux (Pattern p1) (TVar m) false env ;
          inf_aux (Pattern p2) (TList (TVar m)) false env
      end


    | Val v -> begin match v with
        | Const c -> inf_const c t
        | _ -> failwith "this value cannot be typed yet\n"
      end


    | Cpl (e1, e2) ->
      let m = max () in
      let m' = max () in
      prob := (t, TCpl(TVar m, TVar m'))::!prob;
      inf_aux e1 (TVar m) false env ;
      inf_aux e2 (TVar m') false env


    | Aff (e1, e2) ->
      prob := (t, TUnit)::!prob;
      let m = max () in
      inf_aux e2 (TVar m) false env ;
      inf_aux e1 (TRef (TVar m)) false env

    | Der e ->
      let m = max () in
      prob := (t, TVar m)::!prob;
      inf_aux e (TRef (TVar m)) false env


    | If (b, et, ef) ->
      let m = max (); in
      prob := (t, TVar m)::!prob;
      inf_aux b (TBool) false env;
      inf_aux et (TVar m) false env;
      inf_aux ef (TVar m) false env


    | Match (e, l) ->
      let t_e = TVar (max ()) in
      let st_e = make_empty_schema t_e in
      inf_aux e t_e false env;
      List.iter (fun (p, eo) ->
          let env' = add_pat_to_tenv p st_e env in
          inf_aux eo t false env'
        ) l;


    | Cons(e1, e2) ->
      let m = max () in
      inf_aux e1 (TVar m) false env;
      inf_aux e2 (TList (TVar m)) false env;
      prob := (t, TList (TVar m))::!prob


    | Try (e, p, e') ->
      inf_aux e t in_top_level env;
      let env' = add_pat_to_tenv p (make_empty_schema TInt) env in
      inf_aux e' t false env'


    | Raise e -> inf_aux e TInt false env

  in
  inf_aux e (TVar x) true env


(** fonction principale *)
let inference e =
  (* initialisation
   * prob = ens des contraintes
   * toplevel = ens des identifiants déclarés en surface
   * env = ens des types des variables connues, au debut de la stdlib *)
  let prob = ref [] in
  let toplevel = ref [] in
  let env = (List.map (fun (s,t) -> s, make_schema t) StdLib.types_stdlib) in

  (* fonction qui permet de donner un numero unique à chaque variable de type *)
  let max_ = ref 0 in
  let max () =
    incr max_ ; !max_
  in

  (* calcul du type de e : comme un let x = e in x *)

  inference_polymorphe e prob toplevel env max 0 ;

  let types = Unification.unification !prob in

  let t = find_type 0 types in
  let env' = update_env types env in
  let st = ( match e,t with _ , TRef _ | App _ , _ -> make_empty_schema | _ -> generalize env' ) t in

  (* resultats *)

  toplevel := (Ident "-", st) :: !toplevel;
  toplevel := update_env types !toplevel ;
  types, !prob,  !toplevel
