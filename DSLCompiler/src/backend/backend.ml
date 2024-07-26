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
            variable_name ^ ".data[" ^ (lower_bexpression expr) ^ "]"
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
			(lower_bexpression v) ^ " = " ^ (lower_bexpression expr) ^ ";"
	| BVariableDeclaration(Array2(dim1, dim2), name) ->
			(* Array2 declaration happens in two parts: first,
			we need to put the array for the acutal storage space on the
			stack, then we put the struct on.  *)
			"float " ^ (bvar_name_of name) ^ "_data" ^ "[" ^ (bvar_name_of dim1) ^ "*" ^ (bvar_name_of dim2) ^ "];\n" ^
			"array2 " ^ (bvar_name_of name) ^ ";\n" ^
			(bvar_name_of name) ^ ".data = " ^ (bvar_name_of name) ^ "_data" ^ ";\n" ^
			(bvar_name_of name) ^ ".x_dim = " ^ (bvar_name_of dim1) ^ ";\n" ^
			(bvar_name_of name) ^ ".y_dim = " ^ (bvar_name_of dim1) ^ ";\n"

	| BVariableDeclaration(Array1(dim1), name) ->
			"array1 " ^ (bvar_name_of name) ^ "[" ^ (bvar_name_of dim1) ^ "];"
	| BVariableDeclaration(Float, name) ->
			"float " ^ (bvar_name_of name) ^ ";"
	| BLoop(Range(itervar, boundvar), bir) ->
			let i = lower_bvar itervar in
			let n = lower_bvar boundvar in
			String.concat [
				"for (int "; i; " = 0; "; i; " < "; n; "; "; i; "++) {";
				(lower_backend_ir options bir);
				"}"
			]

let variable_to_defs variable = "array2 " ^ variable

(* Flatten the IR *)
let rec flatten_backend_ir backend_ir =
	(* helper function to get a single level flattened.  *)
	let inner_flatten expr = (
		match expr with
		| BSequence(irs) -> irs
		| x -> [x]
	) in
	match backend_ir with
	| BSequence(ir) ->
			let flattened = List.map ~f:flatten_backend_ir ir in
			BSequence(List.concat (List.map ~f:inner_flatten flattened))
	| BLoop(range, bir) ->
			BLoop(range, flatten_backend_ir bir)
	| x -> x

(* Assume a copy between two 2D arrays *)
let copy_between_variables v1 v2 =
	let i_iter = new_temp_variable () in
	let j_iter = new_temp_variable () in
	let i_bound = (bvar_name_of v1) ^ ".x_dim" in
	let j_bound = (bvar_name_of v2) ^ ".y_dim" in
	BLoop(Range(BVar(i_iter), BVar(i_bound)),
	  BLoop(Range(BVar(j_iter), BVar(j_bound)),
	    BAssign(
			BIndex(v1, (mult_dimensional_access i_iter (BVar(i_bound)) j_iter)),
			BIndex(v2, (mult_dimensional_access i_iter (BVar(i_bound)) j_iter))
		)
	  )
	)

(* main lowering entry point *)
let lower options mast =
	let res_var, backend_ir = mast_to_backend_ir options mast in
	(* Add a copy to the output variable.  *)
	let full_backend_ir = BSequence([backend_ir] @ [copy_between_variables (BVar("output_variable")) res_var]) in
	(* Optimize the backend_ir... *)
	let flattened = flatten_backend_ir full_backend_ir in

	let result = lower_backend_ir options flattened in
	(* Add a function wrapper for the code snippet.  *)
	"void f(" ^ (String.concat ~sep:", " (List.map ~f:variable_to_defs (Midend.set_of_variables mast))) ^ ", array2 output_variable) {" ^ result ^ "}"
