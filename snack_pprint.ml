open Snack_ast
open Format

let id fmt ident = fprintf fmt "%s" ident;;
let kwd fmt s = fprintf fmt "%s" s;;
let pInt fmt i = fprintf fmt "%i" i;;
let pFloat fmt f = fprintf fmt "%f" f;;
let pBool fmt b= fprintf fmt "%B" b;;
let indent = 4;;

type op = Binop of Snack_ast.binop | Unop of Snack_ast.unop;;

module SnackPrecMap = Map.Make(struct type t = op let compare = compare end)
let precs = SnackPrecMap.empty;;
let precs = SnackPrecMap.add (Binop Op_or) 1 precs;;
let precs = SnackPrecMap.add (Binop Op_and) 2 precs;;
let precs = SnackPrecMap.add (Unop Op_not) 3 precs;;
let precs = SnackPrecMap.add (Binop Op_eq) 4 precs;;
let precs = SnackPrecMap.add (Binop Op_neq) 4 precs;;
let precs = SnackPrecMap.add (Binop Op_lt) 4 precs;;
let precs = SnackPrecMap.add (Binop Op_gt) 4 precs;;
let precs = SnackPrecMap.add (Binop Op_gteq) 4 precs;;
let precs = SnackPrecMap.add (Binop Op_lteq) 4 precs;;
let precs = SnackPrecMap.add (Binop Op_add) 5 precs;;
let precs = SnackPrecMap.add (Binop Op_sub) 5 precs;;
let precs = SnackPrecMap.add (Binop Op_mul) 6 precs;;
let precs = SnackPrecMap.add (Binop Op_div) 6 precs;;
let precs = SnackPrecMap.add (Unop Op_minus) 7 precs;;

let rec print_program fmt prog =
	open_vbox 0;
	match prog with
	| [] -> ()
	| {header; decls; stmts} :: tail ->
		open_vbox indent;
			print_header fmt header;
			print_decls fmt decls;
			print_cut();
			print_stmts fmt stmts;
		close_box(); print_cut();
		kwd fmt "end"; print_cut(); print_cut();
		print_program fmt tail;
	close_box();

and print_header fmt (ident, args) =
	open_box 0;
		kwd fmt "proc"; print_space();
		id fmt ident; print_space(); kwd fmt "(";
		print_args fmt args; kwd fmt ")";
	close_box();

and print_args fmt args =
	match args with
	| arg :: [] ->
		print_arg fmt arg;
	| arg :: tail ->
		print_arg fmt arg;
		kwd fmt ","; print_space();
		print_args fmt tail;
	| [] -> ();

and print_arg fmt arg =
	match arg with
	| Val (ident, t) ->
		kwd fmt "val"; print_space();
		print_type fmt t; print_space();
		id fmt ident;
	| Ref (ident, t) ->
		kwd fmt "ref"; print_space();
		print_type fmt t; print_space();
		id fmt ident;

and print_type fmt t =
	match t with
	| Bool  -> kwd fmt "bool"
	| Int -> kwd fmt "int"
	| Float -> kwd fmt "float"

and print_decls fmt decls =
	match decls with
	| [] -> ()
	| decl :: tail ->
		print_cut(); open_box 0;
		print_decl fmt decl;
		close_box();
		print_decls fmt tail;

and print_decl fmt decl =
	match decl with
	| Dvar (t, ident) ->
		print_type fmt t; print_space(); id fmt ident; kwd fmt ";";

	| Darr (t, ident, ranges) ->
		print_type fmt t;
		print_space();
		id fmt ident; kwd fmt "[";
		print_ranges fmt ranges;
		kwd fmt "]";
		kwd fmt ";";

and print_ranges fmt ranges =
	match ranges with
	| (r1, r2) :: [] ->
		pInt fmt r1; kwd fmt ".."; pInt fmt r2;
	| (r1, r2) :: tail ->
		pInt fmt r1; kwd fmt ".."; pInt fmt r2;
		kwd fmt ","; print_space();
		print_ranges fmt tail;
	| [] -> ();

and print_paren fmt expr = kwd fmt "("; print_expr fmt expr; kwd fmt ")";

and print_binop_expr fmt op expr =
 match expr with
  | Eunop (op1, expr2) ->
   if (SnackPrecMap.find (Unop op1) precs) <= (SnackPrecMap.find (Binop op) precs) then
    print_paren fmt expr
   else
    print_expr fmt expr2;
  | Ebinop (expr2, op1, exrp3) ->
   if SnackPrecMap.find (Binop op1) precs <= SnackPrecMap.find (Binop op) precs then
    print_paren fmt expr
   else
    print_expr fmt expr;
  | _ -> print_expr fmt expr;

and print_binop_exprs fmt expr1 op expr2 =
 print_binop_expr fmt op expr1;
 print_space(); print_binop fmt op; print_space();
 print_binop_expr fmt op expr2;

and print_exprs fmt exprs =
	match exprs with
	| [] -> ()
	| expr :: tail ->
		print_expr fmt expr;
		print_exprs fmt tail;

and print_expr fmt expr =
	match expr with
	| Ebool value -> pBool fmt value;
	| Eint value -> pInt fmt value;
	| Efloat value -> pFloat fmt value;
	| EId value -> id fmt value;
	| Ebinop (expr1, op, expr2) -> print_binop_exprs fmt expr1 op expr2
	| Eunop (op, expr1) ->
		print_unop fmt op;
		print_expr fmt expr1;
	| Earray (ident, exprs) ->
		id fmt ident;
		kwd fmt "[";
		print_expr_list fmt exprs;
		kwd fmt "]";
 | Eparen expr1 -> print_expr fmt expr1;


and print_expr_list fmt exprs =
	match exprs with
	| expr :: [] ->
		print_expr fmt expr;
	| expr :: tail ->
		print_expr fmt expr;
		kwd fmt ",";
		print_space();
		print_expr_list fmt tail;
	| [] -> ();

and print_binop fmt op =
	match op with
	| Op_add -> kwd fmt "+";
	| Op_sub -> kwd fmt "-";
	| Op_mul -> kwd fmt "*";
	| Op_div -> kwd fmt "/";
	| Op_eq -> kwd fmt "=";
	| Op_neq -> kwd fmt "!=";
	| Op_lt -> kwd fmt "<";
	| Op_gt -> kwd fmt ">";
	| Op_gteq -> kwd fmt ">=";
	| Op_lteq -> kwd fmt "<=";
	| Op_or -> kwd fmt "or";
	| Op_and -> kwd fmt "and";

and print_unop fmt op =
	match op with
	| Op_not -> kwd fmt "not"; print_space();
	| Op_minus -> kwd fmt "-";

and print_lvalue fmt value =
	match value with
	| LId ident -> id fmt ident;
	| Larray (ident, exprs) ->
		id fmt ident; kwd fmt "[";
		print_expr_list fmt exprs; kwd fmt "]";

and print_stmts fmt stmts =
	match stmts with
	| [] -> ()
	| stmt :: tail ->
		print_cut(); open_box 0;
		print_stmt fmt stmt;
		close_box();
		print_stmts fmt tail;

and print_stmt fmt stmt =
	match stmt with
	| Assign (lvalue, rvalue) ->
		print_lvalue fmt lvalue; print_space();
		kwd fmt ":="; print_space();
		print_rvalue fmt rvalue;
		kwd fmt ";";

	| Read lvalue ->
		kwd fmt "read"; print_space();
		print_lvalue fmt lvalue; kwd fmt ";";

	| Write expr ->
		kwd fmt "write"; print_space();
		print_expr fmt expr; kwd fmt ";";

	| WriteS str ->
		kwd fmt "write"; print_space();
		id fmt str; kwd fmt ";";

	| Ifthen (expr, stmts) ->
		open_vbox 0;
			open_vbox indent;
				open_box 0;
					kwd fmt "if"; print_space();
					print_expr fmt expr; print_space();
					kwd fmt "then";
				close_box();
				print_stmts fmt stmts;
			close_box(); print_cut();
			kwd fmt "fi";
		close_box();

	| Ifthenelse (expr, thenStmts, elseStmts) ->
		open_vbox 0;
			open_vbox indent;
				open_box 0;
					kwd fmt "if"; print_space();
					print_expr fmt expr; print_space();
					kwd fmt "then";
				close_box();
				print_stmts fmt thenStmts;
			close_box();
			print_cut();
			open_vbox indent;
				kwd fmt "else";
				print_stmts fmt elseStmts;
			close_box();
			print_cut();
			kwd fmt "fi";
		close_box();

	| While (expr, stmts) ->
		open_vbox 0;
			open_vbox indent;
				open_box 0;
					kwd fmt "while"; print_space();
					print_expr fmt expr; print_space(); kwd fmt "do";
				close_box();
				print_stmts fmt stmts;
			close_box(); print_cut();
			kwd fmt "od";
		close_box();

	| Proccall (ident, exprs) ->
		id fmt ident; kwd fmt "(";
		print_expr_list fmt exprs;
		kwd fmt ");";

and print_rvalue fmt rvalue =
	match rvalue with
	| Rexpr expr -> print_expr fmt expr;
