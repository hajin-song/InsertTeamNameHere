(*
 * snick_pprint.ml
 * Pretty Printer for the SNACK language
 * Skeleton provided as part of assignment
 * Modified By: Beaudan Campbell-Brown, Ha Jin Song, Mengyu Li
 * Last Modified: 08-APR-2017
 *)

open Snick_ast
open Format

(* Indentation uses 4 spaces *)
let indent = 4;;


(* print_program
	* entry for the pretty printer
	* fmt: formatter
	* prog: Parsed Snack program to be pretty printed
	*)
let rec print_program fmt prog =
	open_vbox 0;
	(* Treating program as list of procedures *)
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

(* print_header
	* prints the procedure header
	* fmt: formatter
	* signature: identifier and list of arguments
	*)
and print_header fmt (ident, args) =
	(* get arguments as list and concatenate them using comma *)
	fprintf fmt "proc %s (%s)" ident (String.concat ", " (get_args args))

(* get_args
	* recursively collect arguemnts and convert into list of args
	* args: list of argument
	*)
and get_args args =
	match args with
	| [] -> [];
	| arg :: tail ->
		arg_string arg :: get_args tail;

(* arg_string
	* convert parsed argument into string
	* arg: argument to convert to string
	*)
and arg_string arg =
	match arg with
	| Val (ident, t) ->
		sprintf "val %s %s" (type_string t) ident
	| Ref (ident, t) ->
		sprintf "ref %s %s" (type_string t) ident

(*
	* type_string
	* convert parsed type into string
	* t: type to convert to string
	*)
and type_string t =
	match t with
	| Bool  -> "bool"
	| Int -> "int"
	| Float -> "float"

(*	print_decls
	* recursively print declarations, treating it as list
	* fmt: formatter
	* decls: list of parsed declarations
	*)
and print_decls fmt decls =
	match decls with
	| [] -> ()
	| decl :: tail ->
		print_decl fmt decl; print_decls fmt tail;

(* print_decl
	*	print a declaration as string
	* fmt: foramtter
	* decl: declaration to print as string
	*)
and print_decl fmt decl =
	match decl with
	| Dvar (t, ident) ->
		fprintf fmt "@,%s %s;" (type_string t) ident;
	| Darr (t, ident, ranges) ->
		fprintf fmt "@,%s %s[%s];" (type_string t)
																													ident
								   																		(String.concat ", " (get_ranges ranges));

(* get_ranges
	* recursively convert ranges into string
	* ranges: ranges to convert
	*)
and get_ranges ranges =
	match ranges with
	| [] -> []
	| (r1, r2) :: tail ->
		sprintf "%i..%i" r1 r2 :: get_ranges tail

(*	print_with_assoc
	* Prints operation with parentheses preserved
	* expr: expression
	* prec1: precendent of the nearby operation
	*								position of the operation in question
	*								changes based on the expr's placement
	*)
and print_with_assoc expr prec1 =
	match expr with
	| Ebinop (_, (_, prec2, _), _, _) ->
		if prec2 <= prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| Eunop ((_, prec2, _), _, _) ->
		if prec2 <= prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| _ -> expr_string expr

(*	print_without_assoc
	*	same as print_with_assoc except the parentheses are
	* preserved if and only if op1's precedence explictly
	* preceeds op2's precedence
	*)
and print_without_assoc expr prec1 =
	match expr with
	| Ebinop (_, (_, prec2, _), _, _) ->
		if prec2 < prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| Eunop ((_, prec2, _), _, _) ->
		if prec2 < prec1 then sprintf "(%s)" (expr_string expr)
		else expr_string expr
	| _ -> expr_string expr

(*	print_binop_Expr
	*	prints expr with binary operator
	* exprl: expression on left hand side
	* binop: contains operator, precedence and association
	* exprr: expression on right hand side
	*)
and print_binop_expr exprl (op, prec, assoc) exprr =
	(* parenthese preservation is affected by the operator's association *)
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

(* print_unop_expr
	* same as print_binop_expr except for unary operator
	* unary operator only has expr (right hand side expression)
	* unop: contains operator, precedence and association
	* expr: expression on right hand side
	*)
and print_unop_expr (op, prec, assoc) expr =
	match assoc with
	| Left_assoc ->	sprintf "%s %s" (unop_string op)
									(print_with_assoc expr prec)
	| Right_assoc -> sprintf "%s %s" (unop_string op)
									 (print_without_assoc expr prec)
	| _ -> sprintf "%s %s" (unop_string op) (expr_string expr)

(* expr_string
	* converts parsed expression as string
	* expr: expression to convert to string
	*)
and expr_string expr =
	match expr with
	| Ebool value -> sprintf "%B" value
	| Eint value -> sprintf "%i" value
	| Efloat value -> sprintf "%f" value
	| EId value -> value
	| Ebinop (expr1, op, expr2, _) -> print_binop_expr expr1 op expr2;
	| Eunop (op, expr1, _) -> print_unop_expr op expr1;
	| Earray (ident, exprs, _) ->
		sprintf "%s[%s]" ident (String.concat ", " (expr_list_string exprs));

(* expr_list_string
	* recursively convert expressions into string form
	* exprs: expressions to convert
	*)
and expr_list_string exprs =
	match exprs with
	| [] -> []
	| expr :: tail ->
	expr_string expr :: expr_list_string tail

(* binop_string
	* binary operators string representation
	* op: operator to format
	*)
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

(* unop_string
	* unary operators string representation
	* op: operator to format
	*)
and unop_string op =
	match op with
	| Op_not -> "not";
	| Op_minus -> "-";

(*	lvalue_string
	*	convert lvalue to string representation
	* value: value to convert
	*)
and lvalue_string value =
	match value with
	| LId ident -> ident
	| Larray (ident, exprs) -> sprintf "%s[%s]" (ident)
								(String.concat ", " (expr_list_string exprs))

(* print_stmts
	* recursively prints list of statements
	* fmt: formatter
	* stmts: list of statements
	*)
and print_stmts fmt stmts =
	match stmts with
	| [] -> ()
	| stmt :: tail ->
		print_cut();
		print_stmt fmt stmt;
		print_stmts fmt tail;

(*	print_stmt
	*	convert parsed statement into string representation
	* fmt: formatter
	* stmt: statement
	*)
and print_stmt fmt stmt =
	match stmt with
	| Assign (lvalue, rvalue) ->
		fprintf fmt "%s := %s;" (lvalue_string lvalue) (expr_string rvalue)

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
		fprintf fmt "%s(%s);" ident (String.concat ", " (expr_list_string exprs))
