open Debug
open Cell
open Sheet

(* Variable pour le mode [paf] : arrête l'exécution si une boucle de
 * dépendance est trouvée *)
let paf = ref false

exception Paf

(* commandes: ce que l'utilisateur peut saisir dans un fichier.
 - La modification d'une cellule avec une nouvelle formule,
 - l'affichage d'une cellule, 
 - l'affichage de toute la feuille *)
type comm = Upd of cellname * form_f | Show of cellname | ShowAll
            | SwitchTo of int


(************ affichage **************)
let show_comm c =
  match c with
  | Upd (c,f) ->
     begin
       ps (cell_name2string c);
       ps"=";
       show_form f
     end
  | Show c ->
     begin
       ps "Show(";
       ps (cell_name2string c);
       ps ")"
     end
  | ShowAll -> ps "ShowAll"
  | SwitchTo i -> (
    ps "SwitchTo ";
    print_int i
  )

(************ faire tourner les commandes **************)

(* exécuter une commande *)
let run_command c = match c with
  | Show cn ->
     begin
       let co = !current_sheet, cellname_to_coord cn in
       eval_p_debug (fun () ->
           "Showing cell "
           ^ cell_name2string cn ^ ": "
         );
       ps (cell_val2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
       print_newline()
     end
  | ShowAll ->
     begin
       eval_p_debug (fun () -> "Show All\n");
       show_sheet !current_sheet
     end
  | SwitchTo i ->
     (
       eval_p_debug (fun () -> "Switching to sheet " ^ (string_of_int i)
                            ^ "\n");
       current_sheet := (i - 1)
     )
  | Upd(cn,form) ->
     let f = !current_sheet, form in
     let co = !current_sheet, cellname_to_coord cn in
     eval_p_debug (fun () -> "Update cell " ^ cell_name2string cn ^ " = " ^ form2string form ^ "\n");
     let c = read_cell co in
     let back_f = c.formula in
     try
       update_cell_formula co f;
       recompute_cell co
     with Dependency_loop ->
       if !paf then raise Paf else
         (
           eval_p_debug (fun () -> "Dependency loop found, reverting\n");
           update_cell_formula co back_f
         )

(* exécuter une liste de commandes *)
let run_script cs = List.iter run_command cs
