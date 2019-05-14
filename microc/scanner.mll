(* Ocamllex scanner for MicroC *)

{ open Microcparse

  module Lex = Lexing
  module Buf = Buffer
 }

(* Regular expression let assignments *)
let digit = ['0' - '9']
let digits = digit+
let letter = ['a'-'z' 'A'-'Z']

(* Pattern matching rules to go from char stream => token. Tokens are defined in microcparse.mly *)
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '%'      { MODULO }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "string" { STRING }
| "char"   { CHAR }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '"'  { SLIT(str (Buf.create 100) lexbuf) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| '\'' (['a'-'z' 'A'-'Z'] as lxm) '\'' {CHLIT(lxm)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and str buf = parse
| [^'"' '\n' '\\']+ as content { Buf.add_string buf content ; str buf lexbuf }
| '\n'      { Buf.add_string buf "\n"; Lex.new_line lexbuf; str buf lexbuf }
| '\\' '"'  { Buf.add_char buf '"'; str buf lexbuf }
| '\\'      { Buf.add_char buf '\\'; str buf lexbuf }
| '"'       { Buf.contents buf } (* return *)

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
