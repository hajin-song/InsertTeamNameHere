open Snick_ast
open Symbol
open Format
let curr_env = ref "main";;

let stack_ptr = ref 0;;

let build_arg arg =
	incr stack_ptr;
	fprintf Format.std_formatter "%s\n" !curr_env;
	match arg with
	| Val (ident, t) ->
		fprintf Format.std_formatter "val = %s\n" ident;
		let symbol = {
			pass_by = Value;
			ident = ident;
			t = t;
			stack = !stack_ptr
		} in
		insert_symbol !curr_env ident (Var symbol);
		symbol;
	| Ref (ident, t) ->
		fprintf Format.std_formatter "ref = %s\n" ident;
		let symbol = {
			pass_by = Reference;
			ident = ident;
			t = t;
			stack = !stack_ptr
		} in
		insert_symbol !curr_env ident (Var symbol);
		symbol;;

let rec build_args args =
	match args with
	| arg :: tail -> (build_arg arg) :: build_args tail;
	| [] -> [];;

let record_proc (ident, args) =
	curr_env := ident;
	stack_ptr := 0;
	let proc_args = build_args args in
	insert_proc {ident = ident; args = proc_args; frame = ref !stack_ptr};;

let rec scan_procs prog =
	match prog with
	| {header = header} :: tail ->
		record_proc header;
		scan_procs tail;
	| [] -> ();;

let do_decl decl =
	let stack = new_ptr !curr_env in
	match decl with
	| Dvar (t, ident) -> let symbol = Var {
			pass_by = Value;
			ident = ident;
			t = t;
			stack = stack
		} in
		insert_symbol !curr_env ident symbol;
	| Darr (t, ident, ranges) -> let symbol = Arr {
			ident = ident;
			t = t;
			ranges = ranges;
			stack = stack
		} in
		insert_symbol !curr_env ident symbol;;
	

let rec do_decls decls =
	match decls with
	| decl :: tail ->
		do_decl decl;
		do_decls tail;
	| [] -> ();;

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

let check_unop unop t =
	match unop, t with
	(* Negation *)
	| Op_minus, Int -> Int
	| Op_minus, Float -> Float
	(* Not *)
	| Op_not, Bool -> Bool
	(* Anything else is invalid *)
	| _ -> print_string "Invalid unary operator type\n"; exit 0;;

let rec expr_type expr =
	match expr with
	| Ebool _ -> Bool
	| Eint _ -> Int
	| Efloat _ -> Float
	| EId ident -> (
		match lookup_symbol !curr_env ident with
		| Var {t = t} -> print_string "Var loaded\n"; t;
		| _ -> print_string "Array identifier used without index\n"; exit 0;
		)
	| Ebinop (lexpr, (binop, _, _), rexpr, id) ->
		let ltype = expr_type lexpr in
		let rtype = expr_type rexpr in
		let newtype = check_binop binop ltype rtype in
		insert_type id newtype;
		newtype;
	| Eunop ((unop, _, _), expr, id) ->
		let t = expr_type expr in
		let newtype = check_unop unop t in
		insert_type id newtype;
		newtype;
	| Earray (ident, exprs, id) -> (
		match lookup_symbol !curr_env ident with
		| Arr {t = t; ranges = ranges} ->
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
		if (expr_type e = Int) then
			check_indicies rs es
		else (print_string "Non-Int index provided for array\n"; exit 0);;

let rec do_proc_args (args : variable list) exprs =
	match args, exprs with
	| [], [] -> ()
	| [], _ -> print_string "Too many procedure arguments provided\n"; exit 0;
	| _, [] -> print_string "Too few procedure arguments provided\n"; exit 0;
	| (a::atail), (e::etail) ->
		let t = expr_type e in
		if t = a.t then
			do_proc_args atail etail
		else (print_string "Incorrect procedure parameter type\n"; exit 0;);;

let do_proc ident exprs =
	let {args = args} = lookup_proc ident in
	do_proc_args args exprs;;

let rec check_coerce t expr =
	let t2 = expr_type expr in
	if t = t2 then
		()
	else (assign_types t t2);

and assign_types lt rt =
	match lt, rt with
	| Int, Int -> ()
	| Float, Float -> ()
	| Bool, Bool -> ()
	| Float, Int -> ()
	| _, _ -> print_string "Invalid assignment types\n"; exit 0;; 

let do_assign lvalue rvalue =
	match lvalue with
	| LId (ident) ->
		(match lookup_symbol !curr_env ident with
		| Var {t = t} -> check_coerce t rvalue;
		| _ -> print_string "Invalid assignment lvalue\n"; exit 0;
		)
	| Larray (ident, exprs) ->
		(match lookup_symbol !curr_env ident with
		| Arr {t = t; ranges = ranges} ->
			check_indicies ranges exprs;
			check_coerce t rvalue;
		| _ -> print_string "Invalid array assignment lvalue\n"; exit 0;
		);;
		

let rec do_stmts stmts =
	match stmts with
	| stmt :: tail ->
		do_stmt stmt;
		do_stmts tail;
	| [] -> ();

and do_stmt stmt =
	match stmt with
	| Assign (lvalue, rvalue) -> do_assign lvalue rvalue;
	| Read lvalue -> print_string "Read\n";
	| Write expr -> let _ = expr_type expr in ();
	| WriteS str -> print_string "WriteS\n";
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
		print_string "Current proc = "; print_string proc; print_string "\n";
		curr_env := proc;
		do_decls decls;
		print_string "Decls scanned\n";
		do_stmts stmts;
		print_string "Stmts scanned\n";
		scan_prog tail;
	| [] -> ();;

let semantic_analysis prog =
	print_string "Starting Analysis\n";
	scan_procs prog;
	print_string "Procs scanned\n";
	scan_prog prog;
	print_string "Prog scanned\n";;