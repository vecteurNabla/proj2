open Types

let debug x = () (* print_string x  *)

type symbol = string

(* Une description consiste soit en un objet construit, soit un pointeur vers une référence de description *)
type descr =
  | Link of shared_types
  | Repr of constr

and constr =
  | Var
  | Op of symbol * shared_types list

(* Et un terme est juste une référence de description *)
and shared_types = descr ref

(* Notez que les variables ne sont pas nommées dans ce type, cela est dû au fait que l'on distingue les variables non plus par leur noms mais par l'adresse du terme de type "t" qui pointe vers cette variable *)

(* La fonction suivante transforme un type en un type avec du partage et 
 * si la liste [defined : (var * shared_types) list] contient la représentation
 * des variables déjà défines, alors la fonction retourne à la fois 
 * l'objet converti (c'est la première composante) et une mise 
 * à jour de la liste des variables (elle est mise à jour dans 
 * le cas où on rencontre pour la première fois une variable 
 * durant la conversion).
 * *)
let rec sharing_of_type defined = function
  | TVar x -> begin
      match List.assoc_opt x defined with
      | None ->
        let t' = ref (Repr Var) in
        t' , (x,t')::defined
      | Some t' ->
        t' , defined
    end

  | TFun(t1,t2) ->
    let t1',defined_ = sharing_of_type defined t1 in
    let t2',defined__ = sharing_of_type defined_ t2 in
    ref (Repr (Op ("fun", [t1'; t2']))), defined__

  | TList t ->
    let t',defined_ = sharing_of_type defined t in
    ref (Repr (Op ("list", [t']))), defined_

  | TInt ->
    ref (Repr (Op ("int", []))), defined
  | TBool ->
    ref (Repr (Op ("bool", []))), defined
  | TUnit ->
    ref (Repr (Op ("unit", []))), defined

(* Cette fonction, dont la description est donnée en énoncé, doit avoir le type : t -> t *)
let rec repr x = match !x with
  | Link y -> (let xx = repr y in x := Link xx ; xx)
  | _ -> x

(* On déclare une exception qu'il faudra utiliser avec "raise" quand le problème n'a pas de solution *)
exception Not_unifyable

(* On peut maintenant donner la fonction d'unification. En utilisant la fonction
   précédente, on se donne directement un bon représentant de chaque terme *)
(* Cette fonction doit avoir le type : t -> t -> unit, elle modifie donc les
   descriptions pointées par t1 et t2 sans renvoyer de résultat *)
let rec unify t1 t2 =
  let r1, r2 = repr t1, repr t2 in
  match !r1, !r2 with
  | Link _, _ | _, Link _ -> failwith "err: Link unexpected"
  | Repr rr1, Repr rr2 ->
    begin match rr1,rr2 with
      | Var, Var when r1 == r2 -> ()
      | Var, _ -> r1 := Link r2
      | _, Var -> r2 := Link r1
      | Op (f,l1), Op (g,l2) when f = g -> List.iter2 unify l1 l2
      | _ ->  raise Not_unifyable
    end


(* Renvoie un booléen indiquant si une représentation de terme "t" est cyclique *)
let cyclic t =
  let rec aux visited t =
    if List.memq t visited then true
    else match !(repr t) with
      | Repr (Op (f, args)) ->
        List.exists (aux (t::visited)) args
      | _ -> false
  in aux [] t

exception Cyclic_type

(* build a giant type with the empty operator to union all the type simultaniously :
 * if there is n constraints x_i = y_i, we build a unique constraint
 * f(x_1...x_n) = f(y_1..y_n) where f is the empty operator
 * This also returns the list defined, containing the variables
 *)
let rec build_megatype defined t1_ t2_ = function
  | [] -> defined, t1_, t2_
  | tt::next ->
    let t1 = fst tt in
    let t2 = snd tt in
    let t1',defined' = sharing_of_type defined t1 in
    let t2',defined'' = sharing_of_type defined' t2 in
    build_megatype defined'' (t1'::t1_) (t2'::t2_) next

(* fonction d'unification
 * prend en paramètre un problème et renvoie la liste
 * des (variables de type, type) après unification.
 * en cas d'échec, les excpetions Not_unifyable ou Cyclic_type peuvent être levées
 *)
let unification (pb:Inference.probleme) =
  let varc = ref 0 in

  let defined, t1_, t2_ = build_megatype [] [] [] pb.ct in
  let t1 = ref (Repr (Op ("", t1_))) in
  let t2 = ref (Repr (Op ("", t2_))) in

  unify t1 t2 ;

  if cyclic t1 || cyclic t2 then raise Cyclic_type ;

  let rec get_type t = match !(repr t) with
    | Link _ -> failwith "it should not be a link\n"
    | Repr r -> begin match r with
        | Var -> varc := !varc + 1 ; TVar (!varc)
        | Op("int", []) -> TInt
        | Op("bool", []) -> TBool
        | Op("unit", []) -> TUnit
        | Op("fun", [u;v]) -> TFun( get_type u, get_type v )
        | Op("list", [u]) -> TList( get_type u )
        | _ -> failwith "unknown operator\n"
      end
  in
  List.map (fun (v,t) -> (v, get_type t)) defined
