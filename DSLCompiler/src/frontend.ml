let parse s =
	let lexbuf = Lexing.from_string s in
	try
		Parser.main Lexer.read lexbuf
	with
	| Parser.Error ->
			failwith ("Parse Error")

(* Take program as input, and produce a typechecked AST.  *)
let do_frontend _options program =
    let parsed = parse program in
    (* TODO -- for some DSLs we should typecheck.  *)
    parsed
