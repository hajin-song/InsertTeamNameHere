open Snack_ast
open Format

let id = print_string;;
let kwd = print_string;;
let pInt = print_int;;
let pFloat = print_float;;
let pBool = print_bool;;
let d = ref 1;;
let indent = 4;;


let rec print_program fmt prog =
	open_vbox 0;
	match prog with
	| [] -> ()
	| {header; decls; stmts} :: tail ->
		open_vbox indent;
			print_header fmt header;
			print_decls fmt decls;
			print_stmts fmt stmts;
		close_box(); print_cut();
		kwd "end"; print_cut(); print_cut();
		print_program fmt tail;
	close_box();

and print_header fmt (ident, args) =
	open_box 0;
		kwd "proc"; print_space();
		id ident; print_space(); kwd "(";
		print_args fmt args; kwd ")";
	close_box();

and print_args fmt args =
	match args with
	| arg :: [] ->
		print_arg fmt arg;
	| arg :: tail ->
		print_arg fmt arg;
		kwd ","; print_space();
		print_args fmt tail;
	| [] -> ();

and print_arg fmt arg =
	match arg with
	| Val (ident, t) ->
		kwd "val"; print_space();
		print_type fmt t; print_space();
		id ident;
	| Ref (ident, t) -> 
		kwd "ref"; print_space();
		print_type fmt t; print_space();
		id ident;

and print_type fmt t =
	match t with
	| Bool  -> kwd "bool"
	| Int -> kwd "int"
	| Float -> kwd "float"

and print_decls fmt decls =
	match decls with
	| [] -> ()
	| decl :: tail ->
		print_decl fmt decl;
		print_decls fmt tail;

and print_decl fmt decl =
	print_cut(); open_box 0;
	match decl with
	| Dvar (t, ident) ->
		print_type fmt t; print_space(); id ident; kwd ";";
		close_box();

	| Darr (t, ident, ranges) ->
		print_type fmt t;
		print_space();
		id ident; kwd "[";
		print_ranges fmt ranges;
		kwd "]";
		kwd ";";
		close_box();

and print_ranges fmt ranges =
	match ranges with
	| (r1, r2) :: [] ->
		pInt r1; kwd ".."; pInt r2;
	| (r1, r2) :: tail ->
		pInt r1; kwd ".."; pInt r2;
		kwd ","; print_space();
		print_ranges fmt tail;
	| [] -> ();

and print_exprs fmt exprs =
	match exprs with
	| [] -> ()
	| expr :: tail ->
		print_expr fmt expr;
		print_exprs fmt tail;

and print_expr fmt expr =
	match expr with
	| Ebool value -> pBool value;
	| Eint value -> pInt value;
	| Efloat value -> pFloat value;
	| EId value -> id value;
	| Ebinop (expr1, op, expr2) ->
		print_expr fmt expr1; print_space();
		print_binop fmt op; print_space();
		print_expr fmt expr2;
	| Eunop (op, expr1) ->
		print_unop fmt op;
		print_expr fmt expr1;
	| Earray (ident, exprs) ->
		id ident;
		kwd "[";
		print_expr_list fmt exprs;
		kwd "]";

and print_expr_list fmt exprs =
	match exprs with
	| expr :: [] ->
		print_expr fmt expr;
	| expr :: tail ->
		print_expr fmt expr;
		kwd ",";
		print_space();
		print_expr_list fmt tail;
	| [] -> ();

and print_binop fmt op =
	match op with
	| Op_add -> kwd "+";
	| Op_sub -> kwd "-";
	| Op_mul -> kwd "*";
	| Op_div -> kwd "/";
	| Op_eq -> kwd "=";
	| Op_neq -> kwd "!=";
	| Op_lt -> kwd "<";
	| Op_gt -> kwd ">";
	| Op_gteq -> kwd ">=";
	| Op_lteq -> kwd "<=";
	| Op_or -> kwd "or";
	| Op_and -> kwd "and";

and print_unop fmt op =
	match op with
	| Op_not -> kwd "not"; print_space();
	| Op_minus -> kwd "-";

and print_lvalue fmt value =
	match value with
	| LId ident -> id ident;
	| Larray (ident, exprs) ->
		id ident; kwd "[";
		print_expr_list fmt exprs; kwd "]";

and print_stmts fmt stmts =
	match stmts with
	| [] -> ()
	| stmt :: tail ->
		print_stmt fmt stmt;
		print_stmts fmt tail;

and print_stmt fmt stmt =
	print_cut(); open_box 0;
	match stmt with
	| Assign (lvalue, rvalue) ->
		print_lvalue fmt lvalue; print_space();
		kwd ":="; print_space();
		print_rvalue fmt rvalue;
		kwd ";";
		close_box();

	| Read lvalue ->
		kwd "read"; print_space();
		print_lvalue fmt lvalue; kwd ";";
		close_box();

	| Write expr ->
		kwd "write"; print_space();
		print_expr fmt expr; kwd ";";
		close_box();

	| WriteS str ->
		kwd "write"; print_space();
		id str; kwd ";";
		close_box();

	| Ifthen (expr, stmts) ->
		open_vbox indent;
			open_box 0;
				kwd "if"; print_space();
				print_expr fmt expr; print_space();
				kwd "then";
			close_box();
			print_stmts fmt stmts;
		close_box(); close_box(); print_cut();
		kwd "fi";

	| Ifthenelse (expr, thenStmts, elseStmts) ->
		open_vbox indent;
			open_box 0;
				kwd "if"; print_space();
				print_expr fmt expr; print_space();
				kwd "then";
			close_box();
			print_stmts fmt thenStmts;
		close_box(); print_cut();
		open_vbox indent;
			kwd "else";
			print_stmts fmt elseStmts;
		close_box(); close_box(); print_cut();
		kwd "fi";

	| While (expr, stmts) ->
		open_vbox indent;
			open_box 0;
				kwd "while"; print_space();
				print_expr fmt expr; print_space(); kwd "do";
			close_box();
			print_stmts fmt stmts;
		close_box(); close_box(); print_cut();
		kwd "od";

	| Proccall (ident, exprs) ->
		id ident; kwd "(";
		print_expr_list fmt exprs;
		kwd ");";
		close_box();

and print_rvalue fmt rvalue =
	match rvalue with
	| Rexpr expr -> print_expr fmt expr;