open Memory
open Expr

exception Div_by_Zero
exception App_not_fun of  expr*value
exception Not_expected of string
exception Unbound of string
exception Match_Failure

(* utilitaires de décapsulation *)
let ( !. ) v = match v with
  | VInt(i) -> i
  | _ -> raise (Not_expected "un entier")

let ( !? ) v = match v with
  | VBoo(a) -> a
  | _ -> raise (Not_expected "une booleen")

let ( !! ) v = match v with
  | VRef(a) -> a
  | _ -> raise (Not_expected "une reference")

let ( !& ) v = match v with
  | VCpl (v1,v2) -> v1
  | _ -> raise (Not_expected "un tuple" )

let ( !&& ) v = match v with
  | VCpl (v1,v2) -> v2
  | _ -> raise (Not_expected "un tuple" )

let ( !* ) v = match v with
  | VList (v1::v2) -> v1
  | _ -> raise (Not_expected "une liste non vide" )

let ( !** ) v = match v with
  | VList (v1::v2) -> VList v2
  | _ -> raise (Not_expected "une liste non vide" )

let ( !*** ) v = match v with
  | VList l -> l
  | _ -> raise (Not_expected "une liste" )

(* trouve dans l'environnemnt la valeur correspondant au nom : value *)
let rec find_var x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find_var x t

(* trouve dans l'environnement les valeurs correspondant au pattern : value *)
let rec find_pattern env = function
  | Under -> raise (Not_expected "une variable")
  | Ident x -> find_var x env
  | PConst c -> const_to_val c
  | PCpl (x1,x2) -> VCpl(find_pattern env x1, find_pattern env x2)
  | PList_cons (h,t) -> VList (find_pattern env h :: !***(find_pattern env t))

(* ajoute a l'environnement env le apttern et les valeurs associées : env *)
let rec add_pattern_to_env env x v = match x with
  | Under -> env
  | Ident x -> (x,v)::env
  | PConst c when const_to_val c <> v -> raise Match_Failure
  | PConst _ -> env
  | PCpl(x,y) -> add_pattern_to_env (add_pattern_to_env env x !&v) y !&&v
  | PList_cons(h,t) -> add_pattern_to_env (add_pattern_to_env env h !*v) t !**v

(* teste si les deux arguments matchent : bool *)
let rec pattern_match p v = match p,v with
  | Under,_ | Ident _,_ -> true
  | PConst c,_ -> const_to_val c = v
  | PCpl(x,y), VCpl(v1,v2) -> pattern_match x v1 && pattern_match y v2
  | PList_cons(h,t) , VList (vh::vt) -> pattern_match h vh && pattern_match t (VList vt)
  | _ -> false

(* sémantique opérationnelle à grands pas : value *)
let rec eval env m e k break = match e with
  | Const c -> k (const_to_val c)

  | Cpl(e1,e2) ->
    eval env m e1 (fun v1 ->
        eval env m e2 (fun v2 ->
            k ( VCpl (v1,v2) )
          )
          break
      )
      break

  | Pattern x -> k (find_pattern env x)

  | Let(x, e1, e2) ->
    (* let v =  eval env m e1 k break in
     * let env' = add_pattern_to_env env x v in
     *  eval env' m e2 k break *)
     eval env m e1 (fun v1 ->
         eval (add_pattern_to_env env x v1) m e2 k break
       ) break

  | Fun(x,e) -> k ( VFun(x,env,e) )
  | Rec(Ident f, e, e') ->
     (* let v =
      *   begin
      *     match  eval env m e k break with
      *     | VFun(x,env',e') ->
      *        let rec v = VFun(x, (f,v)::env', e') in v
      *     | _ -> raise (Not_expected "une fonction")
      *   end
      * in  eval (add_pattern_to_env env (Ident f) v) m e' k break *)
     eval env m e (function
           VFun(x, env', e') ->
           let rec v = VFun(x, (f,v)::env', e') in
           eval (add_pattern_to_env env (Ident f) v) m e' k break
         | _ -> raise (Not_expected "une fonction")
       ) break
  | Rec(_, _, _) -> raise (Not_expected "un nom de fonction recursive")
  | App(e1,e2) ->
    (* begin
     *   let varg = eval env m e2 k break in
     *   let vfun = eval env m e1 k break in
     *   match vfun with
     *   | VFun(x,env',e) ->  eval (add_pattern_to_env env' x varg) m e k break
     *  (\* | VRec(f,x,env',e) -> eval (add_pattern_to_env ((f,vfun)::env') x varg) m e break k *\)
     *   | VStdLib(f) -> f m varg ;
     *   | _ -> raise (App_not_fun (e1,vfun))
     * end *)
     (* eval env m e1 (fun v ->
      *     let varg = eval env m e2 k break in
      *     match v with
      *      | VFun(x,env',e) -> k (eval (add_pattern_to_env env' x varg) m e k break)
      *     (\* | VRec(f,x,env',e) -> eval (add_pattern_to_env ((f,vfun)::env') x varg) m e break k *\)
      *     | VStdLib(f) -> k (f m varg)
      *     | _ -> raise (App_not_fun (e1,v))
      *   ) break *)
    eval env m e2 (fun varg ->
        eval env m e1 (function
         | VFun(x,env',e) -> eval (add_pattern_to_env env' x varg) m e k break
         (* | VRec(f,x,env',e) -> eval (add_pattern_to_env ((f,vfun)::env') x varg) m e break k *)
         | VStdLib(f) -> k (f m varg)
         | vfun -> raise (App_not_fun (e1,vfun))
          )
          break
      )
      break

  | Seq(e1,e2) -> eval env m e1 (fun v -> eval env m e2 k break) break
  | Aff(e1, e2) ->
     (* begin
      *   let v =  eval env m e2 k break  in
      *   let a = !! ( eval env m e1 k break) in
      *   set_mem m a v ;
      *   VUnit
      * end *)
     eval env m e2 (fun v2 ->
         eval env m e1 (fun v1 ->
             set_mem m !!v1 v2; VUnit
           ) break
       ) break
  | Der(e) ->  
    (* begin   
     *   let a = !! ( eval env m e k break) in
     *   try get_mem m a
     *   with Not_found -> raise (Unbound "reference")
     * end *)
      eval env m e (fun v -> try k (get_mem m !!v)
                          with Not_found -> raise (Unbound "reference")
        ) break

  | If(b,e1,e2) ->
     (* if !?( eval env m b k break)
      * then  eval env m e1 k break
      * else  eval env m e2 k break *)
     eval env m b (fun boo -> if !?boo then eval env m e1 k break
                           else eval env m e2 k break) break

  | Match(e,l) ->
      (* let v = eval env m e k break in
       * matching env m v l k break *)
     eval env m e (fun v -> matching env m v l k break) break

  | Cons(e1,e2) ->
    eval env m e2 (function
        | VList t ->
          eval env m e1 (fun h ->
              k (VList(h::t))
            )
            break
        | _ -> raise (Not_expected "une liste")
      )
      break

  | Try (e1,p,e2) ->
    let k' v =  eval (add_pattern_to_env env p v) m e2 k break in
     eval env m e1 break k'
  | Raise e -> eval env m e break break

(* realise le mathcing des differents cas : value *)
and matching env m v l k break = match l with
  | [] -> raise Match_Failure
  | (p, e)::_ when pattern_match p v -> eval (add_pattern_to_env env p v) m e k break
  | _::t -> matching env m v t k break
