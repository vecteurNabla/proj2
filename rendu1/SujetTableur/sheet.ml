(* tableau de cellules *)
open Cell

let size = (20,10) (* lignes, colonnes *)

(* Exception pour les boucles de dépendances *)
exception Dependency_loop

(* le tableau que l'on manipule dans le programme ; *)
(* si nécessaire, tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *)
(* default_cell est défini dans cell.ml (module Cell) *)
let thesheets = Array.init 10
  (fun i -> Array.make_matrix (fst size) (snd size) (default_cell ()))
              
(* Lit la cellule de coordonnées [co] *)
let read_cell coo =
  let s, co = coo in
  thesheets.(s).(fst co).(snd co)

let update_cell_formula co f =
  let c = (read_cell co) in
  (* Trouve les boucles de dépendances *)
  let rec find_loops co' =
    if co' = co then
      raise Dependency_loop
    else
      let new_c = read_cell co' in
      List.iter (find_loops) new_c.dep_o
  in
  (* supression des anciennes dependances *)
  let remove_dep co' =
    let c' = read_cell co' in
    c'.dep_o <- List.filter (fun co'' -> co <> co'') c'.dep_o
  in
  List.iter remove_dep (form2dep c.formula);
  (* ajout des nouvelles *)
  let add_dep co' =
    let c' = read_cell co' in
    c'.dep_o <- co::c'.dep_o
  in
  let new_f_deps = form2dep f in
  List.iter add_dep new_f_deps;
  (* maj de la formule *)
  c.formula <- f;
  (* ici on cherche les boucles *)
  List.iter (find_loops) c.dep_o

let update_cell_value co v = (read_cell co).value <- v


(* exécuter une fonction, f, sur tout le tableau *)
let sheet_iter f =
  for i = 0 to (fst size -1) do
    for j = 0 to (snd size -1) do
      f i j
    done;
  done



(* initialisation du tableau : questions un peu subtiles de partage,
 * demandez autour de vous si vous ne comprenez pas pourquoi cela est
 * nécessaire.
 * Vous pouvez ne pas appeler la fonction ci-dessous,
 * modifier une case du tableau à l'aide de update_cell_formula, et
 * regarder ce que ça donne sur le tableau : cela devrait vous donner
 * une piste *)
let init_sheet s =
  let init_cell i j =
    let c = default_cell () in
    thesheets.(s).(i).(j) <- c
  in
  sheet_iter init_cell

(* on y va, on initialise *)
let _ = for i = 0 to 9 do
          init_sheet i
        done

(* affichage rudimentaire du tableau *)

let show_sheet s =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (s, (i,j)) in
      print_string (cell_val2string c) ;
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()




(********** calculer les valeurs à partir des formules *************)

(* on marque qu'on doit tout recalculer en remplissant le tableau de "None" *)
(*    à faire : mettre tout le monde à None *)
let invalidate_sheet () =
  sheet_iter (fun i j -> (read_cell (!current_sheet, (i, j))).value <- None)
(* Plus utilisée ? *)

(*    à faire : le cœur du programme *)
let rec eval_form fo = match fo with
  | Cst n -> n
  | Cell (s, (p,q)) -> eval_cell (s, (p, q))
  | Op(o,fs) -> (
    match o with
    | S -> List.fold_left (fun a b -> a +: (eval_form b)) _0 fs
    | M -> List.fold_left (fun a b -> a *: (eval_form b)) _1 fs
    | A -> let s,n = List.fold_left (fun a b ->
                        fst a +: (eval_form b), snd a + 1
                      ) (_0, 0) fs in
          s/:(I n)
    | Max -> List.fold_left (fun a b -> max_number a (eval_form b)) _min
              fs
  )
  | Fnc(s, c1, c2) -> let v1, v2 = eval_cell c1,
                                  eval_cell c2 in
                     (read_cell (s, (1,0))).formula <- Cst v1;
                     (read_cell (s, (2,0))).formula <- Cst v2;
                     recompute_cell (s, (0,0));
                     match (read_cell (s, (0,0))).value with
                     | Some x -> x
                     | None -> failwith "Problème lors de l'évaluation"
                     
(* ici un "and", car eval_formula et eval_cell sont a priori
   deux fonctions mutuellement récursives *)
and eval_cell (s, (i, j)) =
  let c = read_cell (s, (i, j)) in
  let x = eval_form c.formula in
  c.value <- Some x; x

(* on recalcule la cellule de coordonnées [co], de la feuille [s] et
 * toutes celles qui en dépendent *)
and recompute_cell co =
  ignore (eval_cell co);
  List.iter (recompute_cell) (read_cell co).dep_o
