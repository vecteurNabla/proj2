(* tableau de cellules *)
open Cell

let size = (20,10) (* lignes, colonnes *)

(* Exception pour les boucles de dépendances *)
exception Dependency_loop
         
(* le tableau que l'on manipule dans le programme ; *)
(* si nécessaire, tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *)
(* default_cell est défini dans cell.ml (module Cell) *)
let thesheet = Array.make_matrix (fst size) (snd size) default_cell
let get co = thesheet.(fst co).(snd co)

let read_cell co = thesheet.(fst co).(snd co)

let update_cell_formula co f =
  let c = (get co) in
  (* Trouve les boucles de dépendances *)
  let t = Hashtbl.create 237 in
  let rec find_loops co =
    if Hashtbl.mem t co then
      raise Dependency_loop
    else
      Hashtbl.add t co ();
      let new_c = get co in
      List.iter (find_loops) new_c.dep
  in
  find_loops co;
  (* supression des anciennes dependances *)
  let remove_dep co' =
    let c' = get co' in
    c'.dep <- List.filter (fun co'' -> co <> co'') c'.dep
  in
  List.iter remove_dep (form2dep c.formula);
  (* ajout des nouvelles *)
  let add_dep co' =
    let c' = get co' in
    c'.dep <- co::c'.dep
  in
  List.iter add_dep (form2dep f);
  (* maj de la formule *)
  c.formula <- f

let update_cell_value co v = (get co).value <- v


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
let init_sheet () =
  let init_cell i j =
    let c = { value = None; formula = Cst 0. ; dep = [] } in
    thesheet.(i).(j) <- c
  in
  sheet_iter init_cell

(* on y va, on initialise *)
let _ = init_sheet ()


(* affichage rudimentaire du tableau *)

let show_sheet () =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
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
  sheet_iter (fun i j -> (read_cell (i, j)).value <- None)


(*    à faire : le cœur du programme *)
let rec eval_form fo = match fo with
  | Cst n -> n
  | Cell (p,q) -> eval_cell p q
  | Op(o,fs) -> match o with
               | S -> List.fold_left (fun a b -> a +. (eval_form b)) 0.0 fs
               | M -> List.fold_left (fun a b -> a *. (eval_form b)) 1.0 fs
               | A -> let s,n = List.fold_left (fun a b ->
                                   fst a +. (eval_form b), snd a + 1
                                 ) (0.0, 0) fs in
                     s/.float_of_int(n)
               | Max -> List.fold_left (fun a b -> max a (eval_form b)) min_float fs
(* ici un "and", car eval_formula et eval_cell sont a priori
   deux fonctions mutuellement récursives *)
and eval_cell i j =
  let c = read_cell (i, j) in
  let x = eval_form c.formula in
  c.value <- Some x ; x

(* on recalcule la cellule de coordonnées [co], et toutes celles qui
 * en dépendent *)
let rec recompute_cell co =
  ignore (eval_cell (fst co) (snd co));
  List.iter recompute_cell (get co).dep
  
