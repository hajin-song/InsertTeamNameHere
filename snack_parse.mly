/* ocamlyacc parser for bean */
%{
open Snack_ast
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> IDENT
%token PROC VAL REF END
%token BOOL INT FLOAT
%token WRITE READ
%token ASSIGN
%token WHILE DO OD
%token IF THEN ELSE FI
%token LPAREN RPAREN
%token EQ NEQ LT GT GTEQ LTEQ
%token PLUS MINUS MUL DIV
%token AND OR NOT
%token SEMICOLON COMMA
%token EOF

%left OR
%left AND
%left NOT
%nonassoc EQ NEQ LT GT GTEQ LTEQ
%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS

%type <Snack_ast.program> program

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
  | IDENT LPAREN arguments RPAREN { ($1, $3) }

argument :
  | REF typespec IDENT { Ref ($3, $2) }
  | VAL typespec IDENT { Val ($3, $2) }

arguments : 
  | arguments COMMA argument { $3 :: $1 }
  | argument { [$1] }
  | { [] }

decl :
  | typespec IDENT SEMICOLON { ($2, $1) }

decls :
  | decls decl { $2 :: $1 }
  | { [] }

/* Builds stmts in reverse order */
stmts:
  | stmts stmt { $2 :: $1 }
  | { [] }

stmt :
  | stmt_body SEMICOLON { $1 }
  | IF expr THEN stmts FI { Ifthen ($2, $4) }
  | IF expr THEN stmts ELSE stmts FI { Ifthenelse ($2, $4, $6) }
  | WHILE expr DO stmts OD { While ($2, $4) }

stmt_body:
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | lvalue ASSIGN rvalue { Assign ($1, $3) }

rvalue :
  | expr { Rexpr $1 }

lvalue:
  | IDENT { LId $1 }

expr:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | FLOAT_CONST { Efloat $1 }
  | lvalue { Elval $1 }
  /* Binary operators */
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr NEQ expr { Ebinop ($1, Op_neq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | expr GT expr { Ebinop ($1, Op_gt, $3) }
  | expr LTEQ expr { Ebinop ($1, Op_lteq, $3) }
  | expr GTEQ expr { Ebinop ($1, Op_gteq, $3) }
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | LPAREN expr RPAREN { $2 }
