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

let mbinop_eq binop1 binop2 =
    match binop1, binop2 with
    | (MPlus, MPlus) -> true
    | (MTimes, MTimes) -> true
    | (MMinus, MMinus) -> true
    | _, _ -> false

let rec mexpr_eq expr1 expr2 = 
    match expr1, expr2 with
    | MVar(v1), MVar(v2) -> v1 = v2
    | MBinop(op1, lh1, rh1), MBinop(op2, lh2, rh2) ->
            (mbinop_eq op1 op2) && (mexpr_eq lh1 lh2) && (mexpr_eq rh1 rh2)
    | _, _ -> false

(* Helper function for later stages to determine the set of variables.  *)
let rec set_of_variables mast =
	match mast with
	| MBinop(_op, v1, v2) ->
            let l1 = set_of_variables v1 in
            let l2 = set_of_variables v2 in
            (* Merge the lists -- keeping unique varibales
            only *)
            Utils.unique_merge_lists String.equal l1 l2
    | MVar(v1) -> [v1]
