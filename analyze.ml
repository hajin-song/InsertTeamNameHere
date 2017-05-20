(*
 * analyze.ml
 * Code Anaylser for Snack
 * Created By: Beaudan Campbell-Brown, Ha Jin Song, Mengyu L
 * Modified By: Beaudan Campbell-Brown, Ha Jin Song, Mengyu Li
 * Last Modified: 11-MAY-2017
 *)

open Snick_ast
open Symbol
open Format
let curr_env = ref "main";;

let stack_ptr = ref 0;;

(* check_binop
	* Validates Binary Operator
*)
let check_binop binop ltype rtype =
	match binop, ltype, rtype with
	(* Add *)
	| Op_add, Int, Int -> Int
	| Op_add, Float, Int -> Float
	| Op_add, Int, Float -> Float
	| Op_add, Float, Float -> Float
	(* Subtract *)
	| Op_sub, Int, Int -> Int
	| Op_sub, Float, Int -> Float
	| Op_sub, Int, Float -> Float
	| Op_sub, Float, Float -> Float
	(* Multiply *)
	| Op_mul, Int, Int -> Int
	| Op_mul, Float, Int -> Float
	| Op_mul, Int, Float -> Float
	| Op_mul, Float, Float -> Float
	(* Divide *)
	| Op_div, Int, Int -> Int
	| Op_div, Float, Int -> Float
	| Op_div, Int, Float -> Float
	| Op_div, Float, Float -> Float
	(* Less than *)
	| Op_lt, Int, Int -> Bool
	| Op_lt, Float, Int -> Bool
	| Op_lt, Int, Float -> Bool
	| Op_lt, Float, Float -> Bool
	(* Greater than *)
	| Op_gt, Int, Int -> Bool
	| Op_gt, Float, Int -> Bool
	| Op_gt, Int, Float -> Bool
	| Op_gt, Float, Float -> Bool
	(* Greater than or equal to *)
	| Op_gteq, Int, Int -> Bool
	| Op_gteq, Float, Int -> Bool
	| Op_gteq, Int, Float -> Bool
	| Op_gteq, Float, Float -> Bool
	(* Less than or equal to *)
	| Op_lteq, Int, Int -> Bool
	| Op_lteq, Float, Int -> Bool
	| Op_lteq, Int, Float -> Bool
	| Op_lteq, Float, Float -> Bool
	(* Equal to *)
	| Op_eq, Int, Int -> Bool
	| Op_eq, Float, Float -> Bool
	| Op_eq, Bool, Bool -> Bool
	(* Not equal to *)
	| Op_neq, Int, Int -> Bool
	| Op_neq, Float, Float -> Bool
	| Op_neq, Bool, Bool -> Bool
	(* Or *)
	| Op_or, Bool, Bool -> Bool
	(* And *)
	| Op_and, Bool, Bool -> Bool
	(* Anything else is invalid *)
	| _ -> print_string "Invalid binary operator types\n"; exit 0;;

(*	check_unop
	* Validates Unary Operator
*)
let check_unop unop t =
	match unop, t with
	(* Negation *)
	| Op_minus, Int -> Int
	| Op_minus, Float -> Float
	(* Not *)
	| Op_not, Bool -> Bool
	(* Anything else is invalid *)
	| _ -> print_string "Invalid unary operator type\n"; exit 0;;

(* build_args
	* Builds arguments for proc by determining whether it is
	* PbV or PbR, then allocate to appropriate stack space
	*)
let rec build_args args =
	match args with
	| arg :: tail ->
		let n = !stack_ptr in
		incr stack_ptr;
		build_arg (arg, n) :: build_args tail;
	| [] -> [];
and build_arg (arg, n) =
	match arg with
	| Val (ident, t) ->
		let symbol = {
			pass_by = Value;
			var_ident = ident;
			var_t = t;
			var_stack = n;
			var_is_zero = false;
		} in
		insert_symbol !curr_env ident (Var symbol);
		symbol;
	| Ref (ident, t) ->
		let symbol = {
			pass_by = Reference;
			var_ident = ident;
			var_t = t;
			var_stack = n ;
			var_is_zero = false;
		} in
		insert_symbol !curr_env ident (Var symbol);
		symbol;;

(* scan_procs
	*	Scan and build scope space for procs by reading its identifier and arguments
*)
let rec scan_procs prog =
	match prog with
	| {header = header} :: tail -> record_proc header; scan_procs tail;
	| [] -> ();
and record_proc (ident, args) =
	curr_env := ident;
	stack_ptr := 0;
	let proc_args = build_args args in
	insert_proc {proc_ident = ident; args = proc_args; frame = ref !stack_ptr};;

(* do_decls
	* process proc's declarations and add to proc's environment
*)
let rec do_decls decls =
	match decls with
	| decl :: tail -> do_decl decl; do_decls tail;
	| [] -> ();
and do_decl decl =
	match decl with
	| Dvar (t, ident) -> do_var_decl t ident;
	| Darr (t, ident, ranges) -> do_arr_decl t ident ranges;
and do_var_decl t ident =
	let stack = new_ptr !curr_env in
	let symbol = Var {
		pass_by = Value;
		var_ident = ident;
		var_t = t;
		var_stack = stack ;
		var_is_zero = false;
	} in
	insert_symbol !curr_env ident symbol;
and do_arr_decl t ident ranges =
	let size = arr_size ranges in
	let stack = new_arr_ptr !curr_env size in
	let symbol = Arr {
		arr_ident = ident;
		arr_t = t;
		ranges = ranges;
		arr_stack = stack;
		last_ptr = stack + size - 1
	} in
	insert_symbol !curr_env ident symbol;
and arr_size ranges =
	match ranges with
	| (lo,hi)::tail ->
		if lo > hi then
			(print_string "Invalid array range\n"; exit 0)
		else
			(hi + 1 - lo) * (arr_size tail);
	| [] -> 1;;


let rec expr_value expr =
match expr with
| { expr = Ebool (value); id = id } -> (not value);
| { expr = Eint (value); id = id } -> if value == 0 then true else false;
| { expr = Efloat (value); id = id } -> if value == 0.0 then true else false;
| { expr = EId (ident); id = id } ->
	(
		match lookup_symbol !curr_env ident with
		| Var {var_t = t ; var_is_zero = is_zero } -> is_zero;
		| _ -> print_string "Array identifier used without index\n"; exit 0;
	)
| { expr = Ebinop (lexpr, (binop, _, _), rexpr); id = id } ->
	(expr_value lexpr) && (expr_value rexpr);
| { expr = Eunop ((unop, _, _), expr); id = id } -> (expr_value expr);
| { expr = Earray (ident, exprs); id = id } -> false;;


(* expr_type
	* process expression's type and mark the expression's id to the resulting type
*)
let rec expr_type expr =
	match expr with
	| { expr = Ebool (value); id = id } -> insert_type id Bool; Bool;
	| { expr = Eint (value); id = id } -> insert_type id Int; Int;
	| { expr = Efloat (value); id = id } -> insert_type id Float; Float;
	| { expr = EId (ident); id = id } ->
		(
			match lookup_symbol !curr_env ident with
			| Var {var_t = t} -> insert_type id t; t;
			| _ -> print_string "Array identifier used without index\n"; exit 0;
		)
	| { expr = Ebinop (lexpr, (binop, _, _), rexpr); id = id } ->
		let ltype = expr_type lexpr in
		let rtype = expr_type rexpr in
		let newtype = check_binop binop ltype rtype in
		insert_type id newtype;
		if (expr_value lexpr) || (expr_value rexpr) then
			(print_string "Zero divison\n"; exit 0;)
		else
			newtype;
	| { expr = Eunop ((unop, _, _), expr); id = id } ->
		let t = expr_type expr in
		let newtype = check_unop unop t in
		insert_type id newtype;
		newtype;
	| { expr = Earray (ident, exprs); id = id } ->
		(
			match lookup_symbol !curr_env ident with
			| Arr {arr_t = t; ranges = ranges} ->
			insert_type id t;
			check_indicies ranges exprs;
			t;
			| _ -> print_string "Non-array identifier used as array\n"; exit 0;
		)
and check_indicies ranges exprs =
	match ranges, exprs with
	| [], [] -> ()
	| [], _ -> print_string "Too many array indicies provided\n"; exit 0;
	| _, [] -> print_string "Too few array indicies provided\n"; exit 0;
	| (r::rs), (e::es) ->
		if (expr_type e = Int) then check_indicies rs es else
		(print_string "Non-Int index provided for array\n"; exit 0);;

(* do_proc
* process procedure calls and ensure correct signature is given
*)
let rec do_proc ident exprs =
	let {args = args} = lookup_proc ident in
	check_proc_signature args exprs;
and check_proc_signature (args : variable list) exprs =
	match args, exprs with
	| [], [] -> ()
	| [], _ -> print_string "Too many procedure arguments provided\n"; exit 0;
	| _, [] -> print_string "Too few procedure arguments provided\n"; exit 0;
	| ({pass_by = Reference; var_t = var_t}::atail), ({expr = EId ident} as e::etail) ->
		let t = expr_type e in
		if t = var_t then check_proc_signature atail etail else
		(print_string "Incorrect procedure parameter type\n"; exit 0;);
	| ({pass_by = Value; var_t = var_t}::atail), (e::etail) ->
		let t = expr_type e in
		if t = var_t then check_proc_signature atail etail else
		if t = Int && var_t = Float then check_proc_signature atail etail else
		(print_string "Incorrect procedure parameter type\n"; exit 0;);
	| _ -> print_string "Pass by reference not given a variable\n"; exit 0;;

(* do_assign
	* Check if assignment is valid - Check for value coercion if non-matching type
*)
let rec do_assign lvalue rvalue =
	match lvalue with
	| LId (ident) ->
		let symbol = lookup_symbol !curr_env ident in
			(match symbol with
			| Var {var_t = t} ->
				check_coerce t rvalue;
				update_symbol_value !curr_env ident (expr_value rvalue);
			| _ -> print_string "Invalid assignment lvalue\n"; exit 0;
			)
	| Larray (ident, exprs) ->
		(
			match lookup_symbol !curr_env ident with
				| Arr {arr_t = t; ranges = ranges} ->
					check_indicies ranges exprs;
					check_coerce t rvalue;
				| _ -> print_string "Invalid array assignment lvalue\n"; exit 0;
		);
and check_coerce t expr =
	let t2 = expr_type expr in
	if t = t2 then () else
	(assign_types t t2);
and assign_types lt rt =
	match lt, rt with
	| Int, Int -> ()
	| Float, Float -> ()
	| Bool, Bool -> ()
	| Float, Int -> ()
	| _, _ -> print_string "Invalid assignment types\n"; exit 0;;

(* do_stmts
* process statements and run appropriate analyser for each type of statement
*)
let rec do_stmts stmts =
	match stmts with
	| stmt :: tail ->	do_stmt stmt;	do_stmts tail;
	| [] -> ();
and do_stmt stmt =
	match stmt with
	| Assign (lvalue, rvalue) -> do_assign lvalue rvalue;
	| Read lvalue -> ();
	| Write expr -> let _ = expr_type expr in ();
	| WriteS str -> ();
	| Ifthen (expr, stmts) ->
		if (expr_type expr != Bool) then (print_string "Guard is not a Bool\n"; exit 0);
		do_stmts stmts;
	| Ifthenelse (expr, tstmts, fstmts) ->
		if (expr_type expr != Bool) then (print_string "Guard is not a Bool\n"; exit 0);
		do_stmts tstmts;
		do_stmts fstmts;
	| While (expr, stmts) ->
		if (expr_type expr != Bool) then (print_string "Guard is not a Bool\n"; exit 0);
		do_stmts stmts;
	| Proccall (ident, exprs) -> do_proc ident exprs;;

let rec scan_prog prog =
	match prog with
	| {header = (proc, _); decls = decls; stmts = stmts} :: tail ->
		curr_env := proc;
		do_decls decls;
		do_stmts stmts;
		scan_prog tail;
	| [] -> ();;

let semantic_analysis prog =
	scan_procs prog;
	scan_prog prog;
