open Memory

(* type pour les pattern *)
type pattern = Ident of string | Under | Pcpl of pattern*pattern

type const =
    Int of int
  | Unit
  | Bool of bool
  | Nil

type matchable = P of pattern | C of const

(* un type pour des expressions *)
type expr =
    Const of const                  (* i *)

  | Pattern of pattern                    (* x *)
  | Let of pattern*expr*expr          (* let x = e1 in e2 *)
  | Fun of pattern*expr               (* fun x -> e *)
  | Rec of pattern*expr*expr          (* let rec f x = e in e *)
  | App of expr*expr              (* e1 e2 *)

  | Cpl of expr*expr

  | Seq of expr * expr            (* e1; e2 *)
  | Aff of expr * expr            (* e1 := e2 *)
  | Der of expr                   (* !e *)

  | If of expr*expr*expr         (* if b then e1 else e2 *)

  | Match of expr * (matchable*expr) list

  | Cons of expr*expr

(* type pour les valeurs *)
type value =
  | VInt of int
  | VFun of pattern*env*expr
  | VRec of string*pattern*env*expr
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

(* fonctions d'affichage *)
let rec pattern_to_string = function
  | Under -> "_"
  | Ident x -> x
  | Pcpl (x,y) -> "(" ^ pattern_to_string x ^ "," ^ pattern_to_string y ^")"

let affiche_const = function
  | Int i -> print_int i
  | Bool true -> print_string "true"
  | Bool false -> print_string "false"
  | Unit -> print_string "()"
  | Nil -> print_string "[]"

let affiche_matchable = function
  | P p -> print_string (pattern_to_string p)
  | C k -> affiche_const k

let rec affiche_expr_code e =
  let aff_aux s1 a s2 b s3 =
      begin
	print_string s1;
	affiche_expr_code a;
	print_string s2;
	affiche_expr_code b;
	print_string s3
      end
  in
  match e with
  | Const k -> affiche_const k

  | Pattern x -> print_string (pattern_to_string x)
  | Let(x,e1,e2) -> aff_aux ("let " ^ (pattern_to_string x)  ^ " = ") e1 " in " e2 ""
  | Fun(x,e) -> aff_aux "fun " (Pattern x) " -> " e ""
  | Rec(f,e1, e2) -> aff_aux ("let rec " ^ (pattern_to_string f)  ^ " = " ) e1 " in " e2 ""
  | App(e1,e2) -> aff_aux "" e1 " (" e2 ")"

  | Cpl(e1,e2) -> aff_aux "(" e1 "," e2 ")"

  | Seq(e1,e2) -> aff_aux "" e1 " ; " e2 ""
  | Aff(e1,e2) -> aff_aux "" e1 " := " e2 ""
  | Der(e) -> begin
      print_string "!(" ;
      affiche_expr_code e ;
      print_string ")"
    end

  | If(b,e1,e2) -> begin
      aff_aux "if " b " then " e1 " else " ;
      affiche_expr_code e2
    end

  | Match(e,l) -> begin
      print_string "match " ;
      affiche_expr_code e ;
      print_string " with " ;
      List.iter
        (fun (x, e) ->
           print_string "| " ;
           affiche_matchable x ;
           print_string " -> " ;
           affiche_expr_code e ;
           print_string " " ;
        )
        l
    end

  | Cons(e1,e2) -> aff_aux "(" e1 ")::(" e2 ")"

let rec affiche_expr_tree e =
  let aff_aux s a b =
      begin
	print_string s;
	affiche_expr_tree a;
	print_string ", ";
	affiche_expr_tree b;
	print_string ")"
      end
  in
  match e with
  | Const k -> affiche_const k

  | Pattern x -> print_string (pattern_to_string x)
  | Let(x,e1,e2) -> aff_aux ("Let(" ^ (pattern_to_string x)  ^ ", ") e1 e2
  | Fun(x,e) -> begin
      print_string ("Fun(" ^ (pattern_to_string x) ^ ", ") ;
      affiche_expr_tree e ;
      print_string ")" ;
    end
  | Rec(f,e1, e2) -> begin
      aff_aux ("Rec(" ^ (pattern_to_string f) ^  ", ") e1 e2
    end
  | App(e1,e2) -> aff_aux "App(" e1 e2

  | Cpl(e1,e2) -> aff_aux "Cpl(" e1 e2

  | Seq(e1,e2) -> aff_aux "Seq(" e1 e2
  | Aff(e1,e2) -> aff_aux "Aff(" e1 e2
  | Der(e) -> begin
      print_string "Der(" ;
      affiche_expr_tree e ;
      print_string ")"
    end

  | If(b,e1,e2) -> begin
      print_string "If(" ;
      affiche_expr_tree b ;
      aff_aux ", " e1 e2
    end

  | Match(e,l) -> begin
      print_string "Match(" ;
      affiche_expr_tree e ;
      print_string ", " ;
      List.iter
        (fun (x, e) ->
           print_string "(" ;
           affiche_matchable x ;
           print_string ", " ;
           affiche_expr_tree e ;
           print_string ")"
        )
        l ;
      print_string ")"
    end

  | Cons(e1,e2) -> aff_aux "Cons(" e1 e2

let rec affiche_val = function
  | VInt x -> print_int x
  | VFun _ -> print_string "<fun>"
  | VRec _ -> print_string "<fun rec>"
  | VStdLib _ -> print_string "<fun stdlib>"
  | VUnit -> print_string "()"
  | VRef _ -> print_string "<address>"
  | VBoo true -> print_string "true"
  | VBoo false -> print_string "false"
  | VCpl (v1,v2) ->
    print_string "(" ; affiche_val v1 ; print_string "," ; affiche_val v2 ; print_string ")"
  | VList l -> begin
      List.iter (fun v -> (affiche_val v ; print_string "::")) l ;
      print_string "[]"
    end

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

let rec find_var_name x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find_var_name x t

let rec find env = function
  | Under -> raise (Not_expected "une variable")
  | Ident x -> find_var_name x env
  | Pcpl (x1,x2) -> VCpl(find env x1, find env x2)

let rec add_pattern_to_env env x v = match x with
  | Under -> env
  | Ident x -> (x,v)::env
  | Pcpl(x,y) -> add_pattern_to_env (add_pattern_to_env env x !&v) y !&&v

let rec pattern_match p v = match p,v with
  | Under,_ | Ident _,_ -> true
  | Pcpl(x,y), VCpl(v1,v2) -> pattern_match x v1 && pattern_match y v2
  | _ -> false

(* sémantique opérationnelle à grands pas *)
let rec eval env m = function
  | Const k -> const_to_val k

  | Cpl(e1,e2) -> VCpl(eval env m e1, eval env m e2)

  | Pattern x -> find env x

  | Let(x, e1, e2) ->
    let v = eval env m e1 in
    let env' = add_pattern_to_env env x v in
    eval env' m e2

  | Fun(x,e) -> VFun(x,env,e)
  | Rec(Ident f,e1,e2) -> begin
      match e1 with
      | Fun(x,e) ->
        let v = VRec(f, x, env, e) in
        eval ((f,v)::env) m e2
      | _ -> raise (Not_expected "une fonction")
    end
  | Rec(_, _, _) -> raise (Not_expected "un nom de fonction recursive")
  | App(e1,e2) -> begin
      let varg = eval env m e2 in
      let vfun = eval env m e1 in
      match vfun with
      | VFun(x,env',e)   -> eval (add_pattern_to_env env' x varg) m e
      | VRec(f,x,env',e) -> eval (add_pattern_to_env ((f,vfun)::env') x varg) m e
      | VStdLib(f) -> f m varg ;
      | _ -> raise App_not_fun
    end

  | Seq(e1,e2) -> begin
      ignore (eval env m e1) ;
      eval env m e2
    end
  | Aff(e1, e2) -> begin
      let v = eval env m e2 in
      let a = !! (eval env m e1) in
      set_mem m a v ;
      VUnit
    end
  | Der(e) ->  begin
      let a = !! (eval env m e) in
      try get_mem m a
      with Not_found -> raise (Unbound "reference")
    end

  | If(b,e1,e2) -> if !?(eval env m b) then eval env m e1 else eval env m e2

  | Match(e,l) ->
      let v = eval env m e in
      matching env m v l

  | Cons(e1,e2) -> begin
      let v = eval env m e1 in
      match eval env m e2 with
      | VList t -> VList(v::t)
      | _ -> raise (Not_expected "une liste")
    end

and matching env m v = function
  | [] -> raise Match_Failure
  | (C k, e)::_ when const_to_val k = v -> eval env m e
  | (P p, e)::_ when pattern_match p v -> eval (add_pattern_to_env env p v) m e
  | _::t -> matching env m v t
