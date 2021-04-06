open Memory
open Expr

exception Div_by_Zero
exception App_not_fun of  expr*value
exception Not_expected of string
exception Unbound of string
exception Match_Failure of string

(* utilitaires de décapsulation *)
let ( !. ) v = match v with
  | Const (Int i) -> i
  | _ -> raise (Not_expected "un entier")

let ( !? ) v = match v with
  | Const (Bool b) -> b
  | _ -> raise (Not_expected "une booleen")

let ( !! ) v = match v with
  | Ref a -> a
  | _ -> raise (Not_expected "une reference")

let ( !& ) v = match v with
  | VCpl (v1,v2) -> v1
  | _ -> raise (Not_expected "un tuple" )

let ( !&& ) v = match v with
  | VCpl (v1,v2) -> v2
  | _ -> raise (Not_expected "un tuple" )

let ( !* ) v = match v with
  | VList (v1,_) -> v1
  | _ -> raise (Not_expected "une liste non vide" )

let ( !** ) v = match v with
  | VList (_,v2) -> v2
  | _ -> raise (Not_expected "une liste non vide" )

(* trouve dans l'environnement la valeur correspondant au nom : value *)
let rec find_var x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find_var x t

(* trouve dans l'environnement les valeurs correspondant au pattern : value *)
let rec find_pattern env = function
  | Under -> raise (Not_expected "une variable")
  | Ident x -> find_var x env
  | PConst c -> Const c
  | PCpl (x1,x2) -> VCpl(find_pattern env x1, find_pattern env x2)
  | PList (h,t) -> VList (find_pattern env h, find_pattern env t)

(* teste si la veleur passée en paramètre est la constante : bool *)
let constant_match c = function
  | Const c' -> c = c'
  | _ -> false

(* ajoute a l'environnement env le apttern et les valeurs associées : env *)
let rec add_pattern_to_env env x v = match x with
  | Under -> env
  | Ident x -> (x,v)::env
  | PConst c -> if constant_match c v then env else raise (Match_Failure (" avec la constante " ^ Affichage.const_to_string c))
  | PCpl (x,y) -> add_pattern_to_env (add_pattern_to_env env x !&v) y !&&v
  | PList (h,t) -> add_pattern_to_env (add_pattern_to_env env h !*v) t !**v

(* teste si les deux arguments matchent : bool *)
let rec pattern_match p v = match p,v with
  | Under, _ | Ident _, _ -> true
  | PConst c, _ -> constant_match c v
  | PCpl (x,y), VCpl (v1,v2) -> pattern_match x v1 && pattern_match y v2
  | PList (h,t) , VList (vh,vt) -> pattern_match h vh && pattern_match t vt
  | _ -> false

(* sémantique opérationnelle à grands pas : value *)
let rec eval env m e k break : value = match e with
  | Val v -> k v

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
     eval env m e1 (fun v1 ->
         eval (add_pattern_to_env env x v1) m e2 k break
       ) break

  | Fun(x,e) -> k ( VFun(x,env,e) )
  | Rec(Ident f, e, e') ->
     eval env m e (function
           VFun(x, env', ef) ->
           let rec v = VFun(x, (f,v)::env', ef) in
           eval (add_pattern_to_env env (Ident f) v) m e' k break
         | _ -> raise (Not_expected "une fonction")
       ) break
  | Rec(_, _, _) -> raise (Not_expected "un nom de fonction recursive")
  | App(e1,e2) ->
    eval env m e2 (fun varg ->
        eval env m e1 (function
         | VFun(x,env',e) -> eval (add_pattern_to_env env' x varg) m e k break
         | StdLib f -> eval env m (f m varg) k break
         | vfun -> raise (App_not_fun (e1,vfun))
          )
          break
      )
      break

  | Seq(e1,e2) -> eval env m e1 (fun v -> eval env m e2 k break) break
  | Aff(e1, e2) ->
     eval env m e2 (fun v2 ->
         eval env m e1 (fun v1 ->
             set_mem m !!v1 v2 ; k (Const Unit)
           ) break
       ) break
  | Der(e) ->
      eval env m e (fun v -> try k (get_mem m !!v)
                          with Not_found -> raise (Unbound "reference")
        ) break

  | If(b,e1,e2) ->
     eval env m b (fun boo -> if !?boo then eval env m e1 k break
                           else eval env m e2 k break) break

  | Match(e,l) ->
     eval env m e (fun v -> matching env m v l k break) break

  | Cons(e1,e2) ->
    eval env m e2 (function
        | (VList _ as t) | (Const Nil as t) ->
          eval env m e1 (fun h ->
              k (VList(h,t))
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
  | [] -> raise (Match_Failure ", aucun cas ne matche")
  | (p, e)::_ when pattern_match p v -> eval (add_pattern_to_env env p v) m e k break
  | _::t -> matching env m v t k break
