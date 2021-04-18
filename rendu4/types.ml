type types =
  | TUnit
  | TInt
  | TBool
  | TExn
  | TList of types
  | TRef of types
  | TFun of types*types
  | TCpl of types*types

  | TVar of int
