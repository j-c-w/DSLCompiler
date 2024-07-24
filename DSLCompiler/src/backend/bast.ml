(* This is a closer representation to C code.
   We use this internally within the backend, then
   lower it to C code before outputting it. *)
type bvar =
	(* The upper bound is specified
	   by a variable in all cases for
	   this DSL.  *)
	| BVar of string

type bounds =
	(* Range is iteration var * bound var *)
	| Range of bvar * bvar

type bvariable_type =
	Float
	(* Array of *dimensions* *)
	| Array1 of bvar
	| Array2 of bvar * bvar

type bconst =
    ConstFloat of float

type bexpression =
	(* Represents bvar[expression] *)
	| BIndex of bvar * bexpression
	| BPlus of bexpression * bexpression
	| BTimes of bexpression * bexpression
	| BMinus of bexpression * bexpression
	| BExpVar of bvar
    | BExpConst of bconst

type backend_ir =
	| BLoop of bounds * backend_ir
	| BExpression of bexpression
	| BAssign of bexpression * bexpression
	| BSequence of backend_ir list
	| BVariableDeclaration of bvariable_type * bvar
