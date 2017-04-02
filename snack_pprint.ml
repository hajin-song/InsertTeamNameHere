open Snack_ast
open Format

let id = print_string;;
let kwd = print_string;;
let pInt = print_int;;
let pFloat = print_float;;
let pBool = print_bool;;
let d = ref 1;;
let indent = 4;;

let print_header fmt (ident, args) = (* Print ident and args *) ()

let rec print_ranges fmt ranges =
	match ranges with
	| (r1, r2) :: [] ->
		pInt r1; kwd ".."; pInt r2;
	| (r1, r2) :: tail ->
		pInt r1; kwd ".."; pInt r2;
		kwd ",";
		print_space();
		print_ranges fmt tail;
	| [] -> ();;

let print_decl fmt decl =
	open_box 0;
	match decl with
	| Dvar (Bool, ident) ->
		kwd "bool"; print_space(); id ident; kwd ";";
		close_box(); print_cut();
	| Dvar (Int, ident) ->
		kwd "int"; print_space(); id ident; kwd ";";
		close_box(); print_cut();
	| Dvar (Float, ident) ->
		kwd "float"; print_space(); id ident; kwd ";";
		close_box(); print_cut();
	| Darr (Bool, ident, ranges) ->
		kwd "bool";
		print_space();
		id ident; kwd "[";
		print_ranges fmt ranges;
		kwd "]";
		kwd ";";
		close_box(); print_cut();
	| Darr (Int, ident, ranges) ->
		kwd "int";
		print_space();
		id ident; kwd "[";
		print_ranges fmt ranges;
		kwd "]";
		kwd ";";
		close_box(); print_cut();
	| Darr (Float, ident, ranges) ->
		kwd "float";
		print_space();
		id ident; kwd "[";
		print_ranges fmt ranges;
		kwd "]";
		kwd ";";
		close_box(); print_cut();;

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
	| Ebool value -> pBool value;
	| Eint value -> pInt value;
	| Efloat value -> pFloat value;
	| EId value -> id value;
	| Ebinop (expr1, op, expr2) ->
		print_expr fmt expr1;
		print_binop fmt op;
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
	| Op_add -> print_space(); kwd "+"; print_space();
	| Op_sub -> print_space(); kwd "-"; print_space();
	| Op_mul -> print_space(); kwd "*"; print_space();
	| Op_div -> print_space(); kwd "/"; print_space();
	| Op_eq -> print_space(); kwd "="; print_space();
	| Op_neq -> print_space(); kwd "!="; print_space();
	| Op_lt -> print_space(); kwd "<"; print_space();
	| Op_gt -> print_space(); kwd ">"; print_space();
	| Op_gteq -> print_space(); kwd ">="; print_space();
	| Op_lteq -> print_space(); kwd "<="; print_space();
	| Op_or -> print_space(); kwd "or"; print_space();
	| Op_and -> print_space(); kwd "and"; print_space();

and print_unop fmt op =
	match op with
	| Op_not -> kwd "not"; print_space();
	| Op_minus -> kwd "-";;

let print_lvalue fmt value =
	match value with
	| LId ident -> id ident;
	| Larray (ident, exprs) -> id ident; print_space(); print_expr_list fmt exprs;;

let rec print_stmts fmt stmts =
	match stmts with
	| [] -> ()
	| stmt :: tail -> print_stmt fmt stmt;
					  print_stmts fmt tail;

and print_stmt fmt stmt =
	open_box 0;
	match stmt with
	| Assign (lvalue, rvalue) ->
		print_lvalue fmt lvalue; print_space();
		kwd ":="; print_space();
		kwd ";";
		close_box(); print_cut();

	| Read lvalue ->
		kwd "read"; print_space();
		print_lvalue fmt lvalue; kwd ";";
		close_box(); print_cut();

	| Write expr ->
		kwd "write"; print_space();
		print_expr fmt expr; kwd ";";
		close_box(); print_cut();

	| WriteS str ->
		kwd "write"; print_space();
		id str; kwd ";";
		close_box(); print_cut();

	| Ifthen (expr, stmts) ->
		open_vbox indent;
			open_box 0;
				kwd "if"; print_space();
				print_expr fmt expr; print_space();
				kwd "then";
			close_box(); print_cut();
			print_stmts fmt stmts;
		close_box(); close_box(); print_cut();
		kwd "fi"; print_cut();

	| Ifthenelse (expr, thenStmts, elseStmts) ->
		open_vbox indent;
			open_box 0;
				kwd "if"; print_space();
				print_expr fmt expr; print_space();
				kwd "then";
			close_box(); print_cut();
			print_stmts fmt thenStmts;
		close_box(); print_cut();
		open_vbox indent;
			kwd "else";
			print_cut();
			print_stmts fmt elseStmts;
		close_box(); close_box(); print_cut();
		kwd "fi"; print_cut();

	| While (expr, stmts) ->
		open_vbox indent;
			open_box 0;
				kwd "while"; print_space();
				print_expr fmt expr; print_space(); kwd "do";
			close_box(); print_cut();
			print_stmts fmt stmts;
		close_box(); close_box(); print_cut();
		kwd "od"; print_cut();

	| Proccall (ident, exprs) ->
		id ident; kwd "(";
		print_expr_list fmt exprs;
		kwd ");";
		close_box(); print_cut();;

let rec print_program fmt prog =
	match prog with
	| [] -> ()
	| {header; decls; stmts} :: tail ->
		print_header fmt header;
		open_vbox 0;
		print_decls fmt decls;
		print_stmts fmt stmts;
		close_box();
		print_program fmt tail;;