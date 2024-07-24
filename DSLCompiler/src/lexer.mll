{
open Parser
}

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf }
  | ['A'-'z']+ as lxm { VAR (lxm) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | eof { EOF }
  | _ { failwith "unexpected character" }
