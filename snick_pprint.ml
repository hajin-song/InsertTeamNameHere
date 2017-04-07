open Snick_ast
open Format

let indent = 4;;

let rec print_program fmt prog =
	open_vbox 0;
	match prog with
	| [] -> ()
	| {header = header; decls = decls; stmts = stmts} :: tail ->
		open_vbox indent;
			print_header fmt header;
			print_decls fmt decls;
			print_cut();
			print_stmts fmt stmts;
		close_box();
		fprintf fmt "@,end@,@,";
		print_program fmt tail;
	close_box();

and print_header fmt (ident, args) =
		fprintf fmt "proc %s (%s)" ident (String.concat ", " (get_args args))

and get_args args =
	match args with
	| [] -> [];
	| arg :: tail ->
		arg_string arg :: get_args tail;

and arg_string arg =
	match arg with
	| Val (ident, t) ->
		sprintf "val %s %s" (type_string t) ident
	| Ref (ident, t) ->
		sprintf "ref %s %s" (type_string t) ident

and type_string t =
	match t with
	| Bool  -> "bool"
	| Int -> "int"
	| Float -> "float"

and print_decls fmt decls =
	match decls with
	| [] -> ()
	| decl :: tail ->
		print_decl fmt decl;
		print_decls fmt tail;

and print_decl fmt decl =
	match decl with
	| Dvar (t, ident) ->
		fprintf fmt "@,%s %s;" (type_string t) ident;
	| Darr (t, ident, ranges) ->
		fprintf fmt "@,%s %s[%s];" (type_string t)
									ident
								   (String.concat ", " (get_ranges ranges));

and get_ranges ranges =
	match ranges with
	| [] -> []
	| (r1, r2) :: tail ->
		sprintf "%i..%i" r1 r2 :: get_ranges tail

and print_with_assoc expr prec1 =
	match expr with
	| Ebinop (_, (_, prec2, _), _) ->
		if prec2 <= prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| Eunop ((_, prec2, _), _) ->
		if prec2 <= prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| _ -> expr_string expr

and print_without_assoc expr prec1 =
	match expr with
	| Ebinop (_, (_, prec2, _), _) ->
		if prec2 < prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| Eunop ((_, prec2, _), _) ->
		if prec2 < prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| _ -> expr_string expr

and print_binop_expr exprl (op, prec, assoc) exprr =
	match assoc with
	| Left_assoc ->
		sprintf "%s %s %s" (print_without_assoc exprl prec)
						   (binop_string op)
						   (print_with_assoc exprr prec)
	| Right_assoc ->
		sprintf "%s %s %s" (print_with_assoc exprl prec)
						   (binop_string op)
						   (print_without_assoc exprr prec)
	| _ ->
		sprintf "%s %s %s" (expr_string exprl)
						   (binop_string op)
						   (expr_string exprr)

and print_unop_expr (op, prec, assoc) expr =
	match assoc with
	| Left_assoc ->	sprintf "%s %s" (unop_string op)
									(print_with_assoc expr prec)
	| Right_assoc -> sprintf "%s %s" (unop_string op)
									 (print_without_assoc expr prec)
	| _ -> sprintf "%s %s" (unop_string op) (expr_string expr)

and expr_string expr =
	match expr with
	| Ebool value -> sprintf "%B" value
	| Eint value -> sprintf "%i" value
	| Efloat value -> sprintf "%f" value
	| EId value -> value
	| Ebinop (expr1, op, expr2) -> print_binop_expr expr1 op expr2;
	| Eunop (op, expr1) -> print_unop_expr op expr1;
	| Earray (ident, exprs) ->
		sprintf "%s[%s]" ident (String.concat ", " (expr_list_string exprs));

and expr_list_string exprs =
	match exprs with
	| [] -> []
	| expr :: tail ->
		expr_string expr :: expr_list_string tail

and binop_string op =
	match op with
	| Op_add -> "+";
	| Op_sub -> "-";
	| Op_mul -> "*";
	| Op_div -> "/";
	| Op_eq -> "=";
	| Op_neq -> "!=";
	| Op_lt -> "<";
	| Op_gt -> ">";
	| Op_gteq -> ">=";
	| Op_lteq -> "<=";
	| Op_or -> "or";
	| Op_and -> "and";

and unop_string op =
	match op with
	| Op_not -> "not";
	| Op_minus -> "-";

and lvalue_string value =
	match value with
	| LId ident -> ident
	| Larray (ident, exprs) -> sprintf "%s[%s]" (ident)
								(String.concat ", " (expr_list_string exprs))

and print_stmts fmt stmts =
	match stmts with
	| [] -> ()
	| stmt :: tail ->
		print_cut();
		print_stmt fmt stmt;
		print_stmts fmt tail;

and print_stmt fmt stmt =
	match stmt with
	| Assign (lvalue, rvalue) ->
		fprintf fmt "%s := %s;" (lvalue_string lvalue) (rvalue_string rvalue)

	| Read lvalue ->
		fprintf fmt "read %s;" (lvalue_string lvalue)

	| Write expr ->
		fprintf fmt "write %s;" (expr_string expr)

	| WriteS str ->
		fprintf fmt "write \"%s\";" str

	| Ifthen (expr, stmts) ->
		fprintf fmt "@[<v 0>@[<v %d>if %s then" indent (expr_string expr);
		print_stmts fmt stmts;
		fprintf fmt "@]@,fi@]"

	| Ifthenelse (expr, thenStmts, elseStmts) ->
		fprintf fmt "@[<v 0>@[<v %d>if %s then" indent (expr_string expr);
		print_stmts fmt thenStmts;
		fprintf fmt "@]@,@[<v %d>else" indent;
		print_stmts fmt elseStmts;
		fprintf fmt "@]@,fi@]"

	| While (expr, stmts) ->
		fprintf fmt "@[<v 0>@[<v %d>while %s do" indent (expr_string expr);
		print_stmts fmt stmts;
		fprintf fmt "@]@,od@]";

	| Proccall (ident, exprs) ->
		fprintf fmt "%s(%s)" ident (String.concat ", " (expr_list_string exprs))

and rvalue_string rvalue =
	match rvalue with
	| Rexpr expr -> expr_string expr;
