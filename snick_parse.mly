/* snick_parse.mly
 * Parser for SNACK language
 * Skeleton provided as part of assignment
 * Modified By: Beaudan Campbell-Brown, Ha Jin Song, Mengyu Li
 * Last Modified: 08-APR-2017
 */

/* ocamlyacc parser for bean */
%{
open Snick_ast
let expr_count = ref 0

exception Parse_error of string
let parse_error msg = Printf.eprintf "%s\n" msg

%}
/* TOKENS */
%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> IDENT
%token <string> STRING
%token PROC VAL REF END
%token BOOL INT FLOAT
%token WRITE READ
%token ASSIGN
%token WHILE DO OD
%token IF THEN ELSE FI
%token LPAREN RPAREN LBRACKET RBRACKET
%token EQ NEQ LT GT GTEQ LTEQ
%token PLUS MINUS MUL DIV RANGE
%token AND OR NOT
%token SEMICOLON COMMA QUOTE
%token EOF

/* OPERATOR PRECEDENT */
%left OR
%left AND
%left NOT
%nonassoc EQ NEQ LT GT GTEQ LTEQ RANGE
%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS

%type <Snick_ast.program> program

%start program
%%

program: procs { List.rev $1 }

procs:
  | procs proc { $2 :: $1 }
  | { [] }

proc : PROC header decls stmts END { { header = $2 ; decls = List.rev $3 ; stmts = List.rev $4 } }

typespec :
  | BOOL { Bool }
  | INT { Int }
  | FLOAT { Float }

header :
  | IDENT LPAREN arguments RPAREN { ($1, List.rev $3) }

argument :
  | REF typespec IDENT { Ref ($3, $2) }
  | VAL typespec IDENT { Val ($3, $2) }

arguments :
  | arguments COMMA argument { $3 :: $1 }
  | argument { [$1] }
  | { [] }

decl :
  | typespec IDENT SEMICOLON { Dvar ($1, $2) }
  | typespec IDENT LBRACKET range_list RBRACKET SEMICOLON { Darr ($1, $2, $4) }

decls :
  | decls decl { $2 :: $1 }
  | { [] }

range :
  | INT_CONST RANGE INT_CONST { ($1, $3) }

range_list :
  | range_list COMMA range { $3 :: $1 }
  | range { [$1] }

/* Builds stmts in reverse order */
stmts:
  | stmts stmt { $2 :: $1 }
  | { [] }

stmt :
  | stmt_body SEMICOLON { $1 }
  | IF expr THEN stmts FI { Ifthen ($2, List.rev $4) }
  | IF expr THEN stmts ELSE stmts FI { Ifthenelse ($2, List.rev $4, List.rev $6) }
  | WHILE expr DO stmts OD { While ($2, List.rev $4) }

stmt_body:
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | WRITE STRING { WriteS $2 }
  | lvalue ASSIGN expr { Assign ($1, $3) }
  | IDENT LPAREN expr_list RPAREN { Proccall ($1, List.rev $3) }

lvalue:
  | IDENT { LId $1 }
  | IDENT LBRACKET arr_list RBRACKET { Larray ($1, List.rev $3) }

expr_list:
  | expr_list COMMA expr { $3 :: $1 }
  | expr { [$1] }
  | { [] }

arr_list:
  | arr_list COMMA expr { $3 :: $1 }
  | expr { [$1] }

/* Operator altered to include their precendents and associations*/
expr:
  | BOOL_CONST { incr expr_count; { expr = Ebool $1; id = !expr_count } }
  | INT_CONST { incr expr_count; { expr = Eint $1; id = !expr_count } }
  | FLOAT_CONST { incr expr_count; { expr = Efloat $1; id = !expr_count } }
  | IDENT { incr expr_count; { expr = EId $1; id = !expr_count} }
  | IDENT LBRACKET arr_list RBRACKET { incr expr_count; { expr = Earray ($1, List.rev $3); id = !expr_count } }
  /* Binary operators */
  | expr PLUS expr {incr expr_count; { expr = Ebinop ($1, (Op_add, Prec_addsub, Left_assoc), $3); id = !expr_count } }
  | expr MINUS expr {incr expr_count; { expr = Ebinop ($1, (Op_sub, Prec_addsub, Left_assoc), $3); id = !expr_count } }
  | expr MUL expr {incr expr_count; { expr = Ebinop ($1, (Op_mul, Prec_muldiv, Left_assoc), $3); id = !expr_count } }
  | expr DIV expr {incr expr_count; { expr = Ebinop ($1, (Op_div, Prec_muldiv, Left_assoc), $3); id = !expr_count } }
  | expr EQ expr {incr expr_count; { expr = Ebinop ($1, (Op_eq, Prec_eq, Non_assoc), $3); id = !expr_count } }
  | expr NEQ expr {incr expr_count; { expr = Ebinop ($1, (Op_neq, Prec_eq, Non_assoc), $3); id = !expr_count } }
  | expr LT expr {incr expr_count; { expr = Ebinop ($1, (Op_lt, Prec_eq, Non_assoc), $3); id = !expr_count } }
  | expr GT expr {incr expr_count; { expr = Ebinop ($1, (Op_gt, Prec_eq, Non_assoc), $3); id = !expr_count } }
  | expr LTEQ expr {incr expr_count; { expr = Ebinop ($1, (Op_lteq, Prec_eq, Non_assoc), $3); id = !expr_count } }
  | expr GTEQ expr {incr expr_count; { expr = Ebinop ($1, (Op_gteq, Prec_eq, Non_assoc), $3); id = !expr_count } }
  | expr OR expr {incr expr_count; { expr = Ebinop ($1, (Op_or, Prec_or, Left_assoc), $3); id = !expr_count } }
  | expr AND expr {incr expr_count; { expr = Ebinop ($1, (Op_and, Prec_and, Left_assoc), $3); id = !expr_count } }
  | NOT expr {incr expr_count; { expr = Eunop ((Op_not, Prec_not, Left_assoc), $2); id = !expr_count } }
  | MINUS expr %prec UMINUS {incr expr_count; { expr = Eunop ((Op_minus, Prec_uminus, Non_assoc), $2); id = !expr_count } }
  | LPAREN expr RPAREN { $2 }
