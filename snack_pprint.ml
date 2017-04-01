open Snack_ast
open Format

let id = print_string;;
let kwd = print_string;;
let d = ref 1;;

let print_header fmt (ident, args) = (* Print ident and args *) ()

let print_decl fmt decl =
	match decl with
	| Dvar (Bool, ident) ->
		kwd "bool"; print_space(); id ident; kwd ";\n";
	| Dvar (Int, ident) ->
		kwd "int"; print_space(); id ident; kwd ";\n";
	| Dvar (Float, ident) ->
		kwd "float"; print_space(); id ident; kwd ";\n";
	| Darr (Bool, ident, ranges) ->
		kwd "bool";
		print_space();
		id ident; kwd "[";
		(* Print ranges *)
		kwd "]";
		kwd ";\n";
	| Darr (Int, ident, ranges) ->
		kwd "int";
		print_space();
		id ident; kwd "[";
		(* Print ranges *)
		kwd "]";
		kwd ";\n";
	| Darr (Float, ident, ranges) ->
		kwd "float";
		print_space();
		id ident; kwd "[";
		(* Print ranges *)
		kwd "]";
		kwd ";\n";;

let rec print_decls fmt decls =
	match decls with
	| [] -> ()
	| decl :: tail ->
		print_decl fmt decl;
		print_decls fmt tail;;

let rec print_exprs fmt exprs =
	match exprs with
	| [] -> ()
	| expr :: tail ->
		print_expr fmt expr;
		print_exprs fmt tail;

and print_expr fmt expr =
	match expr with
	| Ebool value -> ()
	| Eint value -> ()
	| Efloat value -> ()
	| EId value -> print_string value;
	| Ebinop (expr1, op, expr2) ->
		print_expr fmt expr1;
		(* Print operator *)
		print_expr fmt expr2;
	| Eunop (op, expr1) -> (* Print operator *) print_expr fmt expr1;
	| Earray (ident, exprs) -> (* Print identifier *) print_exprs fmt exprs;;

let print_lvalue fmt value =
	match value with
	| LId ident -> (* Print ident *) ()
	| Larray (ident, exprs) -> (* Print ident *) print_exprs fmt exprs;;

let rec print_stmts fmt stmts =
	match stmts with
	| [] -> ()
	| stmt :: tail -> print_stmt fmt stmt;
					  print_stmts fmt tail;

and print_stmt fmt stmt =
	match stmt with
	| Assign (lvalue, rvalue) -> ()
	| Read lvalue -> ()
	| Write expr -> (* Print write *) print_expr fmt expr;
	| WriteS str -> (* Print write str *) ()
	| Ifthen (expr, stmts) ->
		(* Print if *) print_expr fmt expr;
		(* Print then *)
		print_stmts fmt stmts;
	| Ifthenelse (expr, thenStmts, elseStmts) ->
		(* Print if *) print_expr fmt expr;
		(* Print then *)
		print_stmts fmt thenStmts;
		(* Print else *)
		print_stmts fmt elseStmts;
	| While (expr, stmts) ->
		print_expr fmt expr;
		print_stmts fmt stmts;
	| Proccall (ident, exprs) -> (* Print ident *) print_exprs fmt exprs;;

let rec print_program fmt prog =
	match prog with
	| [] -> ()
	| {header; decls; stmts} :: tail ->
		print_header fmt header;
		open_hovbox !d;
		incr d;
		print_decls fmt decls;
		print_stmts fmt stmts;
		close_box();
		decr d;
		print_program fmt tail;;