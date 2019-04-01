/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

(* Token declarations. The tokens with a <type> indicate the ones with additional data, e.g. BLIT has an OCaml boolean associated with it *)
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT
%token EOF

(* Entry point is the rule called 'program'.*)
(* After parsing we end up with an OCaml type of <Ast.program> i.e. a pair of 2 lists var decl and func decl*)
%start program
%type <Ast.program> program

(* Associativity and Precendence. Ordered from lowest to highest precendence *)
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

(* PRODUCTION RULES *)
(* A ton of these look confusing because they're all recursively defined, but they're essentially just rules that enable breaking down the program just like a CFG keeps producing non-terminals *)
(* Source: https://courses.softlab.ntua.gr/compilers/2015a/ocamlyacc-tutorial.pdf*)

(* Highest level/entry point. Declarations then end-of-file *)
program:
  decls EOF { $1 }

(* Declarations. Recursively splits program into variable declarations and function declarations. This corresponds to the program type we see in ast.ml *)
(* At each point, the pattern is looking to categorize a declaration as eithe a variable or a function declaration, it adds it to the first or second hal of the pair respectively *)
decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

(* Function declaration pattern. Translates literal function header to OCaml struct for function *)
fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

(* Optional formal arguments (the args being passed into the function)*)
formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

(* Sequence of formal arguments*)
formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

(* Parsed type => Ast typ *)
typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }

(* Variable Declaration sequence *)
vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

(* Variable Declaration literal pattern*)
vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

(* Statement production rule. *)
stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

(* Optional expression rule, useful for void functions. Matches a noexpr if empty, otherwise parses/matches with expr *)
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

(* Expresion production rule. Matches expression token to Ast type with operators and assignments included *)
expr:
    LITERAL          { Literal($1)            }
  | FLIT	           { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }

(* Optional arguments used in an expr (Call type).*)
args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

(* Arguments production rule. Either an expression or a comma-separated sequence of expressions *)
(* NOTE: we have similar rules like args_list, formals_list and vdecl_list, but they're all used in different contexts *)
(* args_list = used in function calls e.g. function(a,b) where a and b are IDs of variables (no types used in function call))
(* formals_list = used in function declaration e.g. int gcd(int a, int b) where we have types for our parameters *)
(* vdecl_list = used to separate declarations/lines of code that are separated by semicolons *)
args_list:a
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
