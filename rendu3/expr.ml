open Memory

type const =
    Int of int
  | Unit
  | Bool of bool
  | Nil

(* type pour les pattern *)
type pattern =
    Ident of string
  | Under
  | PConst of const
  | PCpl of pattern*pattern
  | PList_cons of pattern*pattern

(* un type pour des expressions *)
type expr =
    Const of const                  (* i *)

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

(* type pour les valeurs *)
type value =
  | VInt of int
  | VFun of pattern*env*expr
  (* | VRec of string*pattern*env*expr *)
  | VUnit
  | VRef of address
  | VBoo of bool
  | VStdLib of (value mem -> value -> value)
  | VCpl of value*value
  | VList of value list
and env = (string*value) list

let const_to_val = function
  | Int i -> VInt i
  | Bool b -> VBoo b
  | Unit -> VUnit
  | Nil -> VList []

exception Div_by_Zero
exception App_not_fun
exception Not_expected of string
exception Unbound of string
exception Match_Failure

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


let rec find_var x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find_var x t

let rec find_pattern env = function
  | Under -> raise (Not_expected "une variable")
  | Ident x -> find_var x env
  | PConst c -> const_to_val c
  | PCpl (x1,x2) -> VCpl(find_pattern env x1, find_pattern env x2)
  | PList_cons (h,t) -> VList (find_pattern env h :: !***(find_pattern env t))

let rec add_pattern_to_env env x v = match x with
  | Under -> env
  | Ident x -> (x,v)::env
  | PConst c when const_to_val c <> v -> raise Match_Failure
  | PConst _ -> env
  | PCpl(x,y) -> add_pattern_to_env (add_pattern_to_env env x !&v) y !&&v
  | PList_cons(h,t) -> add_pattern_to_env (add_pattern_to_env env h !*v) t !**v

let rec pattern_match p v = match p,v with
  | Under,_ | Ident _,_ -> true
  | PConst c,_ -> const_to_val c = v
  | PCpl(x,y), VCpl(v1,v2) -> pattern_match x v1 && pattern_match y v2
  | PList_cons(h,t) , VList (vh::vt) -> pattern_match h vh && pattern_match t (VList vt)
  | _ -> false

(* s�mantique op�rationnelle � grands pas *)
let rec eval env m k = function
  | Const c -> const_to_val c

  | Cpl(e1,e2) -> VCpl(eval env m k e1, eval env m k e2)

  | Pattern x -> find_pattern env x

  | Let(x, e1, e2) ->
    let v = eval env m k e1 in
    let env' = add_pattern_to_env env x v in
    eval env' m k e2

  | Fun(x,e) -> VFun(x,env,e)
  | Rec(Ident f, e, e') ->
     let v =
       begin
         match eval env m k e with
         | VFun(x,env',e') ->
            let rec v = VFun(x, (f,v)::env', e') in v
         | _ -> raise (Not_expected "une fonction")
       end
     in eval (add_pattern_to_env env (Ident f) v) m k e'
  | Rec(_, _, _) -> raise (Not_expected "un nom de fonction recursive")
  | App(e1,e2) -> begin
      let varg = eval env m k e2 in
      let vfun = eval env m k e1 in
      match vfun with
      | VFun(x,env',e)   -> eval (add_pattern_to_env env' x varg) m k e
      (* | VRec(f,x,env',e) -> eval (add_pattern_to_env ((f,vfun)::env') x varg) m k e *)
      | VStdLib(f) -> f m varg ;
      | _ -> raise App_not_fun
    end

  | Seq(e1,e2) -> begin
      ignore (eval env m k e1) ;
      eval env m k e2
    end
  | Aff(e1, e2) -> begin
      let v = eval env m k e2 in
      let a = !! (eval env m k e1) in
      set_mem m a v ;
      VUnit
    end
  | Der(e) ->  begin
      let a = !! (eval env m k e) in
      try get_mem m a
      with Not_found -> raise (Unbound "reference")
    end

  | If(b,e1,e2) -> if !?(eval env m k b) then eval env m k e1 else eval env m k e2

  | Match(e,l) ->
      let v = eval env m k e in
      matching env m k v l

  | Cons(e1,e2) -> begin
      match eval env m k e2 with
      | VList t ->
        let v = eval env m k e1 in
        VList(v::t)
      | _ -> raise (Not_expected "une liste")
    end

  | Try (e1,p,e2) ->
    let k' v = eval (add_pattern_to_env env p v) m k e2 in
    eval env m k' e1
  | Raise e -> let v = eval env m k e in k v

and matching env m k v = function
  | [] -> raise Match_Failure
  | (p, e)::_ when pattern_match p v -> eval (add_pattern_to_env env p v) m k e
  | _::t -> matching env m k v t
