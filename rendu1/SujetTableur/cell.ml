(* les nombres avec lesquels on calcule *)
type number = F of float | I of int
                              
(* La table courante *)
let current_sheet = ref 0         

let _0 = I 0
let _1 = I 1
let _min = I min_int

let ( +: ) n1 n2 = match n1,n2 with
  | I x, I y -> I (x+y)
  | I x, F y | F y, I x -> F ( y +. (float_of_int x) )
  | F x, F y -> F (x+.y)

let ( *: ) n1 n2 = match n1,n2 with
  | I x, I y -> I (x*y)
  | I x, F y | F y, I x -> F ( y *. (float_of_int x) )
  | F x, F y -> F (x*.y)

let ( /: ) n1 n2 = match n1,n2 with
  | I x, I y ->
    if x mod y = 0 then I (x/y)
    else F ( (float_of_int x) /. (float_of_int y) )
  | I x, F y -> F ( (float_of_int x) /. y )
  | F x, I y -> F ( x /. (float_of_int y) )
  | F x, F y -> F (x/.y)

let max_number n1 n2 = match n1,n2 with
  | I x, I y -> I (max x y)
  | I x, F y | F y, I x -> F (max y (float_of_int x) )
  | F x, F y -> F (max x y)

let print_number = function
  | F x -> print_float x
  | I x -> print_int x

let string_of_number = function
  | F x -> string_of_float x
  | I x -> string_of_int x

(* deux coordonnées, p.ex. ("B",7) *)
type cellname = string*int
type coord = int * (int*int)

(* les deux fonctions ci-dessous sont a reprendre, un jour ou l'autre :
 * elles ne marchent que pour des noms de colonnes ne comportant qu'un
 * caractère *)
let cellname_to_coord cn =
  if String.length (fst cn) > 1 then
    failwith "cellname_to_coord : désolé, je ne sais pas faire"
  else let column = int_of_char (fst cn).[0] - 65 in
       !current_sheet, (snd cn -1, column)

let coord_to_cellname coords =
  let co = snd coords in
  let column_nbr = snd co in
  if column_nbr > 25 then
    failwith "coord_to_cellname : cela ne devrait pas se produire"
  else
    (String.make 1 (char_of_int (column_nbr + 65)), fst co +1)

(* operations que l'on peut utiliser dans les formules *)
type oper = S | M | A | Max (* sum, multiply, average, maximum *)

(* formules : une valeur, la même valeur qu'une autre cellule, une opération et
 * ses arguments *)
type form = Cst of number | Cell of coord | Op of oper * form list
            | Fnc of int * coord * coord

(* transforme un couple de coord en l'intervalle represente *)
let interval_to_list cos coe =
  let rec build_list coord_cur =
    let co_cur = snd coord_cur in
    if (fst co_cur) > (fst (snd coe)) then []
    else if (snd co_cur) > (snd (snd coe))
    then build_list (!current_sheet, ((fst co_cur) + 1, snd (snd cos)))
    else (Cell coord_cur)::(build_list (!current_sheet, (fst co_cur, (snd co_cur) + 1)))
  in
  build_list cos

(* cellules *)
(* un type enregistrement
 * "mutable" signifie que l'on pourra modifier le champ
 * pour info, on a  type 'a option = None | Some of 'a (ici, 'a c'est number)
 * cell est un enregistrement avec trois champs, un champ formula de type form,
 * et un champ value contenant soit Some f (avec f un float), soit None
 * dep_o contient la liste des cellules qui dépendent directement de
 * cette cellule.
 *)
type cell = {
    mutable formula : form;
    mutable value : number option;
    mutable dep_o : coord list;
  }

(* cellule par défait : pas de valeur, et la formule correspondante est la constante 0. *)
let default_cell () = {
    formula = Cst _0;
    value = None;
    dep_o = []
  }

(************ affichage **************)
let cell_name2string cn = (fst cn)^(string_of_int (snd cn))

let cell_val2string c = match c.value with
  | None -> "_"
  | Some n -> string_of_number n

let oper2string = function
  | S -> "SUM"
  | M -> "MULT"
  | A -> "AVERAGE"
  | Max -> "MAX"

let ps = print_string

let rec list2string f = function
  | [x] -> f x
  | x::xs ->
     begin
       f x ^ ";" ^ list2string f xs
     end
  | _ -> failwith "show_list: the list shouldn't be empty"

let rec show_list f = function
  | [x] -> f x
  | x::xs ->
     begin
       f x;
       ps";";
       show_list f xs
     end
  | _ -> failwith "show_list: the list shouldn't be empty"

(* convertir une formule en une chaîne de caractères *)
let rec form2string = function
  | Cell c -> cell_name2string (coord_to_cellname c)
  | Cst n -> string_of_number n
  | Op(o,fl) ->
     begin
       (oper2string o) ^ "(" ^ list2string form2string fl ^ ")"
     end
  | Fnc(s, c1, c2) -> "s" ^ (string_of_int (s + 1)) ^ "(" ^
                       (cell_name2string (coord_to_cellname c1)) ^ ";" ^
                         (cell_name2string (coord_to_cellname c2)) ^ ")"

let rec show_form f = ps (form2string f)

(* renvoie la liste des cellules dont dépend la formule *)
let form2dep f =
  let rec make_list acc = function
    | Cst _ -> acc
    | Cell co -> co :: acc
    | Op(_,t) -> List.fold_left make_list acc t
    | Fnc(s, c1, c2) -> c1::c2::acc
  in
  make_list [] f
