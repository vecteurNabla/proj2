open Expr

let arg_init main_transform =
  App(
    main_transform,
    Cpl(
      Fun ( Ident "x", ~~ "x" ),
      Fun ( Ident "x", ~~ "x" )
    )
  )

let rec transform e =
  (* raccourcis utiles *)
  let k = ~~ "k" in
  let kE = ~~ "kE" in
  let pkkE = PCpl(Ident "k",Ident "kE") in
  let kkE = Cpl( k , kE) in
  let two_expr e_fst e_snd e =
    App(
      transform e_fst,
      Cpl(
        Fun( Ident "fst",
             App(
               transform e_snd,
               Cpl(
                 Fun( Ident "snd",
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
      | Const _ | Pattern _ -> App(k, e)

      | Cpl(e1,e2) ->
        two_expr e1 e2 begin
          App( k , Cpl( ~~ "fst", ~~ "snd" ))
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

      (* | Rec(Ident f, e, e') ->
       * 
       * | Rec(_, _, _) -> raise (Not_expected "un nom de fonction recursive") *)

      | App(e1,e2) ->
        two_expr e2 e1 begin
          App(
            App( ~~ "snd",
                 ~~ "fst"
               ),
            kkE
          )
        end

       | Seq(e1,e2) ->
         App(
           transform e1,
           Cpl(
             Fun( Under,
                  App(
                    transform e2,
                    kkE
                  )
                ),
             kE
           )
         )

       | Aff(e1, e2) ->
         two_expr e2 e1 begin
           App ( k , Aff( ~~ "snd", ~~ "fst" ) )
         end

       | Der(e) ->
         App(
           transform e,
           Cpl(
             Fun( Ident "v",
                  App ( k , Der(~~ "v") )
                ),
             kE
           )
         )

       | If(b,e1,e2) ->
         App(
           transform b,
           Cpl(
             Fun( Ident "b",
                  App(
                    If(~~ "b",
                       transform e1,
                       transform e2
                      ) ,
                    kkE
                  )
                ),
             kE
           )
         )

       (* | Match(e,l) -> *)

       | Cons(e1,e2) ->
         two_expr e1 e2 begin
           App( k , Cpl( ~~ "fst", ~~ "snd" ) )
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

      | _ -> failwith "autres constructeurs non supportés\n"

    end )

(* let transform_stdlib_fun f m v =
 *   VFun( PCpl( Ident "k", Ident "kE"), [],  App( ~~ "k", f m v )) *)

(* let rec transform_stdlib = function
 *   | [] -> []
 *   | (name , VStdLib f)::t -> (name, transform_stdlib_fun f) :: transform_stdlib t
 *   | h::t -> h :: transform_stdlib t *)
