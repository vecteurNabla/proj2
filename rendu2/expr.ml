open Memory

(* type pour les variables *)
type var = string option

(* un type pour des expressions *)
type expr =
    Const of int                  (* i *)
  | Add of expr*expr              (* + *)
  | Mul of expr*expr              (* * *)
  | Min of expr*expr              (* - *)
  | Div of expr*expr              (* + *)

  | Var of var                    (* x *)
  | Let of var*expr*expr          (* let x = e1 in e2 *)
  | Fun of var*expr               (* fun x -> e *)
  | Rec of var*expr*expr          (* let rec f x = e in e *)
  | App of expr*expr              (* e1 e2 *)

  | Unit                          (* () *)
  | Seq of expr * expr            (* e1; e2 *)
  | Aff of expr * expr            (* e1 := e2 *)
  | Der of expr                   (* !e *)

  | If of expr*expr*expr         (* if b then e1 else e2 *)

  | And of expr*expr
  | Or of expr*expr
  | Not of expr

  | Leq of expr*expr
  | Geq of expr*expr
  | Lt of expr*expr
  | Gt of expr*expr
  | Eq of expr*expr
  | Neq of expr*expr

  | True
  | False

(* type pour les valeurs *)
type value =
  | VInt of int
  | VFun of var*env*expr
  | VRec of string*var*env*expr
  | VUnit
  | VRef of address
  | VBoo of bool
  | VStdLib of (value mem -> value -> value)
  | VCouple of value*value
and env = (string*value) list


(* fonctions d'affichage *)
let rec affiche_expr e =
  let aff_aux s a b =
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end
  in
  let var_to_string = function
    | Some x -> x
    | None -> "_"
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Min(e1,e2) -> aff_aux "Min(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2

  | Var x -> print_string (var_to_string x)
  | Let(x,e1,e2) -> aff_aux ("Let(" ^ (var_to_string x)  ^ ", ") e1 e2
  | Fun(x,e) -> begin
      print_string ("Fun(" ^ (var_to_string x) ^ ", ") ;
      affiche_expr e ;
      print_string ")" ;
    end
  | Rec(f,e, e') -> begin
      aff_aux ("Rec(" ^ (var_to_string f) ^  ", ") e e'
    end
  | App(e1,e2) -> aff_aux "App(" e1 e2 ;

  | Unit -> print_string "Unit"
  | Seq(e1,e2) -> aff_aux "Seq(" e1 e2
  | Aff(e1,e2) -> aff_aux "Aff(" e1 e2
  | Der(e) -> begin
      print_string "Der(" ;
      affiche_expr e ;
      print_string ")"
    end

  | If(b,e1,e2) -> begin
      print_string "If(" ;
      affiche_expr b ;
      aff_aux ", " e1 e2
    end
  | And(a,b) -> aff_aux "And(" a b ;
  | Or(a,b) -> aff_aux "Or(" a b
  | Not b -> begin
      print_string "Not(" ;
      affiche_expr b ;
      print_string ")"
    end
  | Leq(e1,e2) -> aff_aux "Leq(" e1 e2
  | Geq(e1,e2) -> aff_aux "Geq(" e1 e2
  | Lt(e1,e2) -> aff_aux "Lt(" e1 e2
  | Gt(e1,e2) -> aff_aux "Gt(" e1 e2
  | Eq(e1,e2) -> aff_aux "Eq(" e1 e2
  | Neq(e1,e2) -> aff_aux "Neq(" e1 e2
  | True -> print_string "True"
  | False -> print_string "False"


exception Div_by_Zero
exception App_not_fun
exception Not_expected of string
exception Unbound of string

let ( !. ) v = match v with
  | VInt(i) -> i
  | _ -> raise (Not_expected "un entier")

let ( !? ) v = match v with
  | VBoo(a) -> a
  | _ -> raise (Not_expected "une booleen")

let ( !! ) v = match v with
  | VRef(a) -> a
  | _ -> raise (Not_expected "une reference")

let ( !& ) = function
  | Some x -> x
  | None -> raise (Not_expected "une variable")

let rec find x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find x t

(* sémantique opérationnelle à grands pas *)
let rec eval env m = function
  | Const k -> VInt k
  | Add(e1,e2) -> VInt( !.(eval env m e1) + !.(eval env m e2) )
  | Mul(e1,e2) -> VInt( !.(eval env m e1) * !.(eval env m e2) )
  | Min(e1,e2) -> VInt( !.(eval env m e1) - !.(eval env m e2) )
  | Div(e1,e2) ->
    let deno = !.(eval env m e2) in (* On calcule d'abord le déno pour *)
    if deno = 0 then               (* ne pas avoir à calculer le num pour rien *)
      raise Div_by_Zero
    else
      VInt(!.(eval env m e1) / deno)

  | Var x -> find !&x env
  | Let(Some x,e1,e2) ->
    let v = eval env m e1 in
    eval ((x,v)::env) m e2
  | Let(None, e1, e2) ->
    let _ = eval env m e1 in
    eval env m e2
  | Fun(x,e) -> VFun(x,env,e)
  | Rec(Some f,e1,e2) -> begin
      match e1 with
      | Fun(x,e) ->
        let v = VRec(f, x, env, e) in
        eval ((f,v)::env) m e2
      | _ -> raise (Not_expected "une fonction")
    end
  | Rec(None, _, _) -> raise (Not_expected "un nom de fonction recursive")
  | App(e1,e2) -> begin
      let varg = eval env m e2 in
      let vfun = eval env m e1 in
      match vfun with
      | VFun(Some x,env',e) -> eval ((x,varg)::env') m e
      | VFun(None,env',e) -> eval env' m e
      | VRec(f,Some x,env',e) -> eval ((x,varg)::(f,vfun)::env') m e
      | VRec(f,None,env',e) -> eval ((f,vfun)::env') m e
      | VStdLib(f) -> f m varg ;
      | _ -> raise App_not_fun
    end

  | Unit -> VUnit
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
  | And(a,b) -> VBoo ( !?(eval env m a) && !?(eval env m b) )
  | Or(a,b) -> VBoo ( !?(eval env m a) || !?(eval env m b) )
  | Not b -> VBoo ( not (!?(eval env m b)) )
  | Leq(e1,e2) -> VBoo ( !.(eval env m e1) <= !.(eval env m e2) )
  | Geq(e1,e2) -> VBoo ( !.(eval env m e1) >= !.(eval env m e2) )
  | Lt(e1,e2) -> VBoo ( !.(eval env m e1) < !.(eval env m e2) )
  | Gt(e1,e2) -> VBoo ( !.(eval env m e1) > !.(eval env m e2) )
  | Eq(e1,e2) -> VBoo ( !.(eval env m e1) = !.(eval env m e2) )
  | Neq(e1,e2) -> VBoo ( !.(eval env m e1) <> !.(eval env m e2) )
  | True -> VBoo true
  | False -> VBoo false
