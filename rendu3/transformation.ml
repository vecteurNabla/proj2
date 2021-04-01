open Expr

let (~+) s = "_" ^ s

let id = Fun ( Ident ~+ "x", ~~ ~+ "x" )
let iid = Cpl(id, id)

let depth = ref 0

let rec transform e =
  (* : do not tranform = pattern que l'on ne transforme pas *)
  (* raccourcis utiles *)
  incr depth ;
  let k_str = ~+ "k" ^ string_of_int !depth in
  let kE_str = ~+ "kE" ^ string_of_int !depth in
  let k = ~~ k_str in
  let kE = ~~ kE_str in
  let pkkE = PCpl(Ident k_str ,Ident kE_str) in
  let kkE = Cpl( k , kE) in
  let two_expr e_fst e_snd e =
    App(
      transform e_fst,
      Cpl(
        Fun( Ident ~+ "fst",
             App(
               transform e_snd,
               Cpl(
                 Fun( Ident ~+ "snd",
                      e
                    ),
                 kE
               )
             )
           ),
        kE
      )
    )
  in

  Fun( pkkE , begin
      match e with
      | Pattern p -> begin
          try                     (* traduction des fonctions de la stdlib à 1 argt *)
            let _ = Eval.find_pattern StdLib.simples p in
            App(
              k,
              Fun(
                Ident ~+ "v",
                Fun(
                  pkkE,
                  App(
                    k,
                    App(e, ~~ ~+ "v" )
                  )
                )
              )
            )
          with Eval.Unbound _ | Eval.Not_expected _ -> begin
              try                 (* traduction des fonctions de la stdlib à 2 argts *)
                let _ = Eval.find_pattern StdLib.doubles p in
                App(
                  k,
                  Fun(
                    Ident ~+ "v1",
                    Fun(
                      pkkE,
                      App(
                        k,
                        Fun(
                          Ident ~+ "v2",
                          Fun(
                            pkkE,
                            App(
                              k,
                              App(
                                App(e, ~~ ~+ "v1" ),
                                ~~ ~+ "v2"
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              with Eval.Unbound _ | Eval.Not_expected _ -> (* traduction des patterns classiques *)
                App(k, e)
            end
        end

      | Val _ -> App(k, e)

      | Cpl(e1,e2) ->
        two_expr e1 e2 begin
          App( k , Cpl( ~~ ~+ "fst", ~~ ~+ "snd" ))
        end

      | Let(x, e1, e2) ->
        App(
          transform e1,
          Cpl(
            Fun(x, App (transform e2, kkE)),
            kE
          )
        )

      | Fun(x,e) -> App(k, Fun(x, transform e))

      (* | Rec(f, e1, e2) ->
       *   App(
       *     Fun(
       *       f,
       *       transform e1
       *     ),
       *     Cpl(
       *       Fun( Ident ~+ "v",
       *            Rec(f,
       *                App(
       *                  ~~ ~+ "v",
       *                  Pattern f
       *                ),
       *                App(
       *                  transform e2,
       *                  kkE
       *                )
       *               )
       *          ),
       *       kE
       *     )
       *   ) *)
      (* | Rec(f, e1, e2) ->
       *   App(
       *     transform e1,
       *     Cpl(
       *       Fun( Ident ~+ "v",
       *            Rec(f, ~~ ~+ "v",
       *                App(
       *                  transform e2,
       *                  kkE
       *                )
       *               )
       *          ),
       *       kE
       *     )
       *   ) *)
      (* | Rec(f, e1, e2) ->
       *   App(
       *     Rec(
       *       f,
       *       transform f e1,
       *       Pattern f
       *     ),
       *     Cpl(
       *       Fun(f, App(transform e2, kkE)),
       *       kE
       *     )
       *   ) *)
      | Rec(f, e1, e2) ->
        App(
          transform e1,
          Cpl(
            Fun(f, App(transform e2, kkE)),
            kE
          )
        )

      | App(e1,e2) ->
        two_expr e2 e1 begin
          App(
            App( ~~ ~+ "snd",
                 ~~ ~+ "fst"
               ),
            kkE
          )
        end

       | Seq(e1,e2) ->
         two_expr e1 e2 begin
           App(
             ~~ ~+ "snd" ,
                    kkE
           )
         end

       | Aff(e1, e2) ->
         two_expr e2 e1 begin
           App ( k , Aff( ~~ ~+ "snd", ~~ ~+ "fst" ) )
         end

       | Der(e) ->
         App(
           transform e,
           Cpl(
             Fun( Ident ~+ "v",
                  App ( k , Der(~~ ~+ "v") )
                ),
             kE
           )
         )

       | If(b,e1,e2) ->
         App(
           transform b,
           Cpl(
             Fun( Ident ~+ "b",
                  App(
                    If(~~ ~+ "b",
                       transform e1,
                       transform e2
                      ) ,
                    kkE
                  )
                ),
             kE
           )
         )

       | Match(e,l) ->
         App(
           transform e,
           Cpl(
             Fun( Ident ~+ "v",
                  Match( ~~ ~+ "v",
                         transform_match_list kkE l
                       )
                ),
             kE
           )
         )

       | Cons(e1,e2) ->
         two_expr e2 e1 begin
           App( k , Cons( ~~ ~+ "snd", ~~ ~+ "fst" ) )
         end

       | Try (e1,p,e2) ->
         App(
           transform e1,
           Cpl(
             k,
             Fun(p, App( transform e2 , kkE ) )
           )
         )
       | Raise e ->
         App(
           transform e,
           Cpl( kE, kE )
         )

    end )

and transform_match_list kkE = function
  | [] -> []
  | (x,e)::t -> (x, App( transform e, kkE)) :: transform_match_list kkE t

(* let transform_stdlib f = fun m varg -> transform (f m varg) *)

let main_transform e =
  Let( Ident "main_transform",
       transform e,
       App(
         ~~ "main_transform",
         iid
       )
     )
