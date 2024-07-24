type mbinop = MPlus | MMinus | MTimes

type mexpr =
  | MVar of string
  | MBinop of mbinop * mexpr * mexpr
