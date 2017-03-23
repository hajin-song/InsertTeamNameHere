{
open Snack_parse
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let ident = (alpha | '_') alnum*
rule token = parse
    [' ' '\t']                  { token lexbuf }     (* skip blanks *)
  | '\n'                        { Lexing.new_line lexbuf ; token lexbuf }
  | '-'?digit+'.'digit+ as lxm  { FLOAT_CONST(float_of_string lxm) }
  | '-'?digit+ as lxm           { INT_CONST(int_of_string lxm) }
  (* keywords *)
  | "bool"  { BOOL }
  | "int"   { INT }
  | "true"  { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read"  { READ }
  | "write" { WRITE }
  | "and"   { AND }
  | "or"    { OR }
  | "not"   { NOT }
  | ":="    { ASSIGN }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '='     { EQ }
  | "!="    { NEQ }
  | '<'     { LT }
  | '>'     { GT }
  | "<="    { LTEQ }
  | ">="    { GTEQ }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { MUL }
  | '/'     { DIV }
  | ';'     { SEMICOLON }
  | ident as lxm { IDENT lxm }
  | eof { EOF }
