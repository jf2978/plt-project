(* Abstract Syntax Tree and functions for printing it *)

(* First we start of with what look like type patterns. But they're actually sort of like constructors (I think they're called variants).
They're defining categories where each pattern corresponds to a type name (must be uppercase) and you can
specify how it'll be represented in OCaml e.g. ("5+5" is represented as the triplet Binop(5,+,5) *)

(* SOURCE: https://www.cs.cornell.edu/courses/cs3110/2013sp/lectures/lec04-types/lec04.html *)

(* Binary Operator types*)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod

(* Unary Operator types*)
type uop = Neg | Not

(* Data types *)

type typ = Int | Bool | Float | Void | String | Char | List | Mat


(* Expression pattern types *)
(* Noexpr is there to make the pattern match exhaustive; so it catches the non-expression that was matched *)
type expr =
    Literal of int
  | Fliteral of string
  | Sliteral of string
  | BoolLit of bool
  | Chlit of char
  | ListLit of expr list
  | MatLit of expr list list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

(* Assignment bind type. Basically a tuple of type (typ, string) e.g. "int x" would be the pair (Int, "x") *)
type bind = typ * string * expr

(* Statement pattern types *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

(* Function Declaration Struct *)
(* I thought this was a map/dictionary at first. An instantiation would look something like ...
let x = {typ = Int; fname = "gcd" ...} using an equal sign instead for each value *)
type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

(* Program type. It's a pair with (a) list of variables (typ, string) and (b) list of function declarations *)
type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | Sliteral(l) -> l
  | Chlit(l) -> Char.escaped l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | _ -> "test"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "string"
  | Char -> "char"
  | Mat -> "mat"
  | List -> "list"


let string_of_vdecl (t, id, e) = string_of_typ t ^ " " ^ id ^ string_of_expr e ^ ";\n"

let string_of_formals (_, id, _) = id

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formals fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
