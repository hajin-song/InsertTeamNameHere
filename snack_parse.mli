type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (float)
  | IDENT of (string)
  | BOOL
  | INT
  | FLOAT
  | WRITE
  | READ
  | ASSIGN
  | WHILE
  | DO
  | OD
  | IF
  | THEN
  | ELSE
  | FI
  | LPAREN
  | RPAREN
  | EQ
  | NEQ
  | LT
  | GT
  | GTEQ
  | LTEQ
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | NOT
  | SEMICOLON
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Snack_ast.program
