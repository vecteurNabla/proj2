open Expr

(* factorielle : defintion puis application à une cst *)
(* Let, FunRec, If, Eq, Var, Const, Mul, Min, App *)
let e0 =
  Let(
    "f",
    FunRec(
      "f",
      "x",
      If(
        Eq(
          Var "x",
          Const 0
        ) ,
        Const 1,
        Mul(
          Var "x" ,
          App(
            Var "f",
            Min(
              Var "x",
              Const 1
            )
          )
        )
      )
    ),
    App(
      Var "f",
      Const 3
    )
  )

(* factorielle et compteur : definition puis application pls fois *)
(* Let, FunRec, If, Eq, Var, Const, Mul, Min, App, Fun, Seq, Aff, Add, Der, Unit, "prInt", "ref" *)
let e1 =
  Let(
    "f",
    FunRec(
      "f",
      "x",
      If(
        Eq(
          Var "x",
          Const 0
        ) ,
        Const 1,
        Mul(
          Var "x" ,
          App(
            Var "f",
            Min(
              Var "x",
              Const 1
            )
          )
        )
      )
    ),
    Let("x",
        App(
          Var "ref",
          Const 0
        ),
        Let(
          "g",
          Fun(
            "_",
            Seq(
              Aff(Var "x",
                  App(
                    Var "prInt",
                    Add(
                      Der (
                        Var "x"
                      ),
                      Const 1
                    )
                  )
                 ),
              App(
                Var "prInt",
                App(
                  Var "f",
                  Der (
                    Var "x"
                  )
                )
              )
            )
          ),
          Seq(
            App(
              Var "g",
              Unit
            ),
            Seq(
              App(
                Var "g",
                Unit
              ),
              Seq(
                App(
                  Var "g",
                  Unit
                ),
                Seq(
                  App(
                    Var "g",
                    Unit
                  ),
                  Seq(
                    App(
                      Var "g",
                      Unit
                    ),
                    App(
                      Var "g",
                      Unit
                    )
                  )
                )
              )
            )
          )
        )
       )
  )

(* factorielle : erreur, oubli du rec *)
(* Let, Fun, If, Eq, Var, Const, Mul, Min, App *)
let e2 =
  Let(
    "f",
    Fun(
      "x",
      If(
        Eq(
          Var "x",
          Const 0
        ) ,
        Const 1,
        Mul(
          Var "x" ,
          App(
            Var "f",
            Min(
              Var "x",
              Const 1
            )
          )
        )
      )
    ),
    App(
      Var "f",
      Const 3
    )
  )

(* factorielle : erreur, comparaison à une valeur non constante *)
(* Let, FunRec, If, Eq, Var, Const, Mul, Min, App *)
let e3 =
  Let(
    "f",
    FunRec(
      "f",
      "x",
      If(
        Eq(
          Var "x",
          Var "f"
        ) ,
        Const 1,
        Mul(
          Var "x" ,
          App(
            Var "f",
            Min(
              Var "x",
              Const 1
            )
          )
        )
      )
    ),
    App(
      Var "f",
      Const 3
    )
  )

(* *)
let e4 =
  Let(
    "b",
    App(
      Var "ref",
      False
    ) ,
    Let(
      "_bidouille",
      Fun(
        "x",
        If(
          And(
            Geq(
              Var "x",
              Const 10
            ),
            Not (
              Der(
                Var "b"
              )
            )
          ),
          Seq(
            Aff(
              Var "b",
              True
            ),
            Div(
              Var "x",
              Const 10
            )
          ),
          Seq(
            Aff(
              Var "b",
              False
            ),
            Add(
              Var "x",
              Const 1
            )
          )
        )
      ),
      App (
        Var "prInt",
        App (
          Var "_bidouille",
          Const 11
        )
      )
    )
  )

(* division par zero *)
let e5 =
  Let(
    "b",
    App(
      Var "ref",
      False
    ) ,
    Let(
      "_bidouille",
      Fun(
        "x",
        If(
            Not (
              Der(
                Var "b"
              )
            ),
          Seq(
            Aff(
              Var "b",
              True
            ),
            Div(
              Var "x",
              Var "x"
            )
          ),
          Seq(
            Aff(
              Var "b",
              False
            ),
            Add(
              Var "x",
              Const 10
            )
          )
        )
      ),
      Seq(
        App (
          Var "prInt",
          App (
            Var "_bidouille",
            Const 1
          )
        ),
        Seq(
          App (
            Var "prInt",
            App (
              Var "_bidouille",
              Const 0
            )
          ),
          App (
            Var "prInt",
            App (
              Var "_bidouille",
              Const 0
            )
          )
        )
      )
    )
  )

(* _ *)
let e6 =
  Let("_",
      App(
        Var "prInt",
        Mul(
          Const 1,
          Const 6
        )
      ),
      True
     )


(* _ error *)
let e7 =
  Let("_",
      App(
        Var "prInt",
        Mul(
          Const 1,
          Const 6
        )
      ),
      Var "_"
     )
