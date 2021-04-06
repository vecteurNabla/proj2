type types =
  | TInt
  | TBool
  | TFun of types*types
  | TVar of int
