open Mast

let op_to_mop op =
    match op with
    | Ast.Plus -> MPlus
    | Ast.Times -> MTimes
    | Ast.Minus -> MMinus

let rec ast_to_mast (ast) =
	match ast with
	| Ast.Var(s) -> MVar(s)
	| Ast.Binop(op, lexpr, rexpr) ->
			MBinop(op_to_mop op, ast_to_mast lexpr, ast_to_mast rexpr)

let lower _opts ast =
	(* Convert to mid-end AST and return that. *)
	let mast = ast_to_mast ast in
	(* Optimize mast *)
	mast
