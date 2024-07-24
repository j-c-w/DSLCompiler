open Bast
open Core

let counter = ref 0
let new_temp_variable () =
  let name = "variable_" ^ string_of_int !counter in
  incr counter;
  name

let name_of mexpr = match mexpr with
  | Mast.MVar(v) -> v
  (* Can't have a name for a binop.  *)
  | Mast.MBinop(_, _, _) -> assert false

let bvar_name_of (bvar: bvar): string = match bvar with
  | BVar(name) -> name

let compute_new_dims op input1 input2 =
	match op with
	(* For plus and minus, the dimensions are hte same *)
	| Mast.MPlus ->
			let name = bvar_name_of input1 in
			(name ^ ".x_dim", name ^ ".y_dim")
	| Mast.MMinus ->
			let name = bvar_name_of input1 in
			(name ^ ".x_dim", name ^ ".y_dim")
	(* For times, we take the first dimension of the first
		and the last dimension of the second.  *)
	| Mast.MTimes ->
			let dim1 = (bvar_name_of input1) ^ ".x_dim" in
			let dim2 = (bvar_name_of input2) ^ ".y_dim" in
			(dim1, dim2)

let mult_dimensional_access v1 dim v2: bexpression =
	BPlus(BTimes(BExpVar(BVar(v1)), BExpVar(dim)), BExpVar(BVar(v2)))

let loops_for_op op output lhs rhs =
	match op with
	| Mast.MPlus | Mast.MMinus ->
			(* For plus and minus, iterate over the arrays
		       and subtract or add element by element.  *)
			let iter_var_1 = new_temp_variable() in
			let iter_var_2 = new_temp_variable() in
			let xdim: bvar = BVar((bvar_name_of lhs) ^ ".x_dim") in
			let ydim: bvar = BVar((bvar_name_of lhs) ^ ".y_dim") in
			let inner_expression = (
                match op with
                | Mast.MPlus ->
					(* plus *)
					(* compute lhs[i * y + j] + rhs[i * y - j] *)
					BPlus(
						BIndex(lhs, BPlus(BTimes(BExpVar(BVar(iter_var_1)), BExpVar(ydim)), BExpVar(BVar(iter_var_2)))),
						BIndex(rhs, BPlus(BTimes(BExpVar(BVar(iter_var_1)), BExpVar(ydim)), BExpVar(BVar(iter_var_2))))
					)
                | Mast.MMinus ->
					(* minus *)
					(* compute lhs[i * y + j] - rhs[i * y - j] *)
					BMinus(
						BIndex(lhs, BPlus(BTimes(BExpVar(BVar(iter_var_1)), BExpVar(ydim)), BExpVar(BVar(iter_var_2)))),
						BIndex(rhs, BPlus(BTimes(BExpVar(BVar(iter_var_1)), BExpVar(ydim)), BExpVar(BVar(iter_var_2))))
					)
                | _ -> assert false (* Can't have a non mplus/mminus here due to previous match.  *)
            )
			in
			BLoop(
				Range(BVar(iter_var_1), xdim),
				BLoop(
					Range(BVar(iter_var_2), ydim),
					BAssign(BIndex(BVar(output), BPlus(BTimes(BExpVar(BVar(iter_var_1)), BExpVar(ydim)), BExpVar(BVar(iter_var_2)))),
                        inner_expression
					)
				)
			)
	| Mast.MTimes ->
			(* Implement a matrix multiplication *)
			let iter_var_1 = new_temp_variable() in
			let iter_var_2 = new_temp_variable() in
			let iter_var_3 = new_temp_variable() in
			let loop_temp_var = new_temp_variable() in
			let dim1: bvar = BVar((bvar_name_of lhs) ^ ".x_dim") in
			let dim2: bvar = BVar((bvar_name_of lhs) ^ ".y_dim") in
			let dim3: bvar = BVar((bvar_name_of lhs) ^ ".y_dim") in
			BLoop(
				Range(BVar(iter_var_1), dim1),
				BLoop(
					Range(BVar(iter_var_2), dim2),
					BSequence([
						BVariableDeclaration(Float, BVar(loop_temp_var));
						BAssign(BExpVar(BVar(loop_temp_var)), BExpConst(ConstFloat(0.0)));
						BLoop(
							Range(BVar(iter_var_3), dim3),
							BAssign(BExpVar(BVar(loop_temp_var)),
							  BPlus(
								  BExpVar(BVar(loop_temp_var)),
								  BTimes(
									BIndex(lhs, (mult_dimensional_access iter_var_1 dim3 iter_var_3)),
									BIndex(rhs, (mult_dimensional_access iter_var_3 dim2 iter_var_2))
								  )
							  )
							)
						);
						BAssign(BIndex(BVar(output), BPlus(BTimes(BExpVar(BVar(iter_var_1)), BExpVar(dim2)), BExpVar(BVar(iter_var_2)))),
							BExpVar(BVar(loop_temp_var))
						)
					])
				)
			)

(* Lowering functions from the mast to the backend_ir *)
(* Return the variable, and the expression to compute it
   as a pair.  *)
let rec mast_to_backend_ir options mast =
	match mast with
	| Mast.MVar(v) -> BVar(v), BSequence([])
	| Mast.MBinop(op, lhs, rhs) ->
			(* Get the loops for the lhs, the rhs
			   and put them into temp varibales --
			   then use those to define this loop.
			   *)
			let (lhs_variable, lhs_expression) = mast_to_backend_ir options lhs in
			let (rhs_variable, rhs_expression) = mast_to_backend_ir options rhs in
			(* Get the dimensions of the output.    This depends on what
			   the operator is *)
			let (dim1, dim2) = compute_new_dims op lhs_variable rhs_variable in
			(* Get a new temp variable.  *)
			let temp_variable = new_temp_variable () in
			(* The return program will be: do the LHS, do the RHS, declare the output
			   then do this operation.  *)
			BVar(temp_variable), BSequence([
				lhs_expression;
				rhs_expression;
				BVariableDeclaration(Array2(BVar(dim1), BVar(dim2)), BVar(temp_variable));
				(loops_for_op op temp_variable lhs_variable rhs_variable)
			])

(* Lowering functions from the backend_ir to C code. *)
let lower_bvar b = match b with
	| BVar(s) -> s

let lower_bconst const = match const with
    | ConstFloat(f) -> string_of_float f

let rec lower_bexpression bexp =
	match bexp with
	| BIndex(v, expr) ->
			let variable_name = lower_bvar v in
            variable_name ^ "[" ^ (lower_bexpression expr) ^ "]"
	| BPlus(expr1, expr2) ->
			(lower_bexpression expr1) ^ "+" ^ (lower_bexpression expr2)
	| BTimes(expr1, expr2) ->
			(lower_bexpression expr1) ^ "*" ^ (lower_bexpression expr2)
	| BMinus(expr1, expr2) ->
			(lower_bexpression expr1) ^ "-" ^ (lower_bexpression expr2)
	| BExpVar(bvar) -> lower_bvar bvar
    | BExpConst(const) -> lower_bconst const

let rec lower_backend_ir options backend_ir =
	match backend_ir with
	| BSequence(ir) ->
			String.concat ~sep:";\n" (
				List.map ~f:(lower_backend_ir options) ir
			)
	| BExpression(expr) ->
			lower_bexpression(expr)
	| BAssign(v, expr) ->
			(lower_bexpression v) ^ " = " ^ (lower_bexpression expr)
	| BVariableDeclaration(Array2(dim1, dim2), name) ->
			(* Stack-declare this variable.  It would be
			   good to heap declare the return one (although
			   this code doesn't do that.  *)
			"float " ^ (bvar_name_of name) ^ "[" ^ (bvar_name_of dim1) ^ "*" ^ (bvar_name_of dim2) ^ "];"
	| BVariableDeclaration(Array1(dim1), name) ->
			"float " ^ (bvar_name_of name) ^ "[" ^ (bvar_name_of dim1) ^ "];"
	| BVariableDeclaration(Float, name) ->
			"float " ^ (bvar_name_of name) ^ ";"
	| BLoop(Range(itervar, boundvar), bir) ->
			let i = lower_bvar itervar in
			let n = lower_bvar boundvar in
			String.concat [
				"for (int "; i; "; "; i; " < "; n; "; "; i; "++) {";
				(lower_backend_ir options bir);
				"}"
			]

(* main lowering entry point *)
let lower options mast =
	let _res_var, backend_ir = mast_to_backend_ir options mast in
	(* Optimize the backend_ir... *)
	let result = lower_backend_ir options backend_ir in
	result

