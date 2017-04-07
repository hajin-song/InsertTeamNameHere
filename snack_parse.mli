type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (float)
  | IDENT of (string)
  | STRING of (string)
  | PROC
  | VAL
  | REF
  | END
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
  | LBRACKET
  | RBRACKET
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
  | RANGE
  | AND
  | OR
  | NOT
  | SEMICOLON
  | COMMA
  | QUOTE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Snack_ast.program
