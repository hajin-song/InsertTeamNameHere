val print_program : Format.formatter -> Snack_ast.t -> unit

val print_header : Format.formatter -> Snack_ast.header -> unit

val print_decls : Format.formatter -> Snack_ast.decl list -> unit
val print_decl : Format.formatter -> Snack_ast.decl -> unit

val print_stmts : Format.formatter -> Snack_ast.stmt list -> unit
val print_stmt : Format.formatter -> Snack_ast.stmt -> unit

val print_exprs : Format.formatter -> Snack_ast.expr list -> unit
val print_expr : Format.formatter -> Snack_ast.expr -> unit