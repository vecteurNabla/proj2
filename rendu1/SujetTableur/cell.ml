(* les nombres avec lesquels on calcule *)
type number = float
let print_number = print_float

(* deux coordonnées, p.ex. ("B",7) *)
type cellname = string*int


(* les deux fonctions ci-dessous sont a reprendre, un jour ou l'autre :
 * elles ne marchent que pour des noms de colonnes ne comportant qu'un
 * caractère *)
let cellname_to_coord cn =
  if String.length (fst cn) > 1 then
    failwith "cellname_to_coord : désolé, je ne sais pas faire"
  else let column = int_of_char (fst cn).[0] - 65 in
       (snd cn -1, column)
let coord_to_cellname co =
  let column_nbr = snd co in
  if column_nbr > 25 then
    failwith "coord_to_cellname : cela ne devrait pas se produire"
  else
    (String.make 1 (char_of_int (column_nbr + 65)), fst co +1)


(* operations que l'on peut utiliser dans les formules *)
type oper = S | M | A (* sum, multiply, average *)

(* formules : une valeur, la même valeur qu'une autre cellule, une opération et
 * ses arguments *)
type form = Cst of number | Cell of (int*int) | Op of oper * form list

(* cellules *)
(* un type enregistrement
 * "mutable" signifie que l'on pourra modifier le champ
 * pour info, on a  type 'a option = None | Some of 'a (ici, 'a c'est number) 
 * cell est un enregistrement avec deux champs, un champ formula de type form,
 * et un champ value contenant soit Some f (avec f un float), soit None *)
type cell = { mutable formula : form; mutable value : number option }

(* cellule par défait : pas de valeur, et la formule correspondante est la constante 0. *)
let default_cell = { formula = Cst 0.; value = None }



(************ affichage **************)
let cell_name2string cn = (fst cn)^(string_of_int (snd cn))

let cell_val2string c = match c.value with
  | None -> "_"
  | Some n -> string_of_float n

let oper2string = function
  | S -> "SUM"
  | M -> "MULT"
  | A -> "AVERAGE"

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
  | Cst n -> string_of_float n
  | Op(o,fl) ->
     begin
       (oper2string o) ^ "(" ^ list2string form2string fl ^ ")"
     end

let rec show_form f = ps (form2string f)
