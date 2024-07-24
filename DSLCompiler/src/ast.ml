type binop = Plus | Minus | Times

type expr =
  | Var of string
  | Binop of binop * expr * expr
