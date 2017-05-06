open Snick_ast;;
open Symbol;;
open Format;;

let indent = 4;;

let scope : string Stack.t = Stack.create ();;

let call proc =	Stack.push proc scope;;

let return () = Stack.pop scope;;

let this_scope () = Stack.top scope;;

let generate_expr fmt expr = ()

let print_write fmt expr =
	generate_expr fmt expr;;
	(* match (get_attrs expr).t with
	| Int -> fprintf "call_builtin print_int@,";
	| Float -> fprintf "call_builtin print_real@,";
	| Bool -> fprintf "call_builtin print_bool@,";; *)

let rec generate_stmts fmt stmts =
	match stmts with
	| (stmt::tail) ->
		generate_stmt fmt stmt;
		generate_stmts fmt tail;
	| [] -> ();

and generate_stmt fmt stmt =
	match stmt with
	| Write expr -> print_write fmt expr;
	| _ -> ();;

let print_var fmt stack t =
	match t with
	| Int ->
		fprintf fmt "int_const r0, 0@,";
		fprintf fmt "store %i, r0@," stack;
	| Float ->
		fprintf fmt "real_const r0, 0@,";
		fprintf fmt "store %i, r0@," stack;
	| Bool ->
		fprintf fmt "bool_const r0, 0@,";
		fprintf fmt "store %i, r0@," stack;;

let generate_var fmt (t, ident) =
	match lookup_symbol (this_scope()) ident with
	| Var {stack = stack} -> print_var fmt stack t;
	| _ -> print_string "Not implemented\n"; exit 0;;

let generate_arr fmt arr = ();;

let generate_decl fmt decl =
	match decl with
	| Dvar x -> generate_var fmt x;
	| Darr x -> generate_arr fmt x;;

let rec generate_decls fmt decls =
	match decls with
	| (decl::tail) ->
		generate_decl fmt decl;
		generate_decls fmt tail;
	| [] -> ();
 	close_box();;

let rec generate_args fmt n args =
	match args with
	| (arg::tail) ->
		generate_arg fmt n arg;
		generate_args fmt n tail;
	| [] -> ();

and generate_arg fmt n arg = 
	match arg with
	| Val (_, t) -> print_arg fmt t n;
	| Ref (_, t) -> ();

and print_arg fmt t n =
	match t with
	| Int ->
		fprintf fmt "store %i, r%i@," n n;
	| Float ->
		fprintf fmt "store %i, r%i@," n n;
	| Bool ->
		fprintf fmt "store %i, r%i@," n n;;

let generate_header fmt (ident, args) =
	call ident;
	fprintf fmt "proc_%s:@," ident;
	open_vbox indent;
	fprintf fmt "# prologue@,";
	let {frame = frame} = lookup_proc ident in
	fprintf fmt "push_stack_frame %i@," !frame;
	generate_args fmt 1 args;;

let rec generate_procs fmt prog =
	match prog with
	| {header = header; decls = decls; stmts = stmts} :: tail ->
		generate_header fmt header;
		generate_decls fmt decls;
		generate_stmts fmt stmts;
		generate_procs fmt tail
	| [] -> ();;

let generate prog =
	let fmt = Format.std_formatter in
	open_vbox 0;
	open_vbox indent;
	print_cut();
	fprintf fmt "call proc_main@,";
	fprintf fmt "halt";
	close_box();
	print_cut();
	generate_procs fmt prog;
	close_box();;