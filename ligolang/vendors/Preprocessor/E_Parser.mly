%{
(* Grammar for boolean expressions in preprocessing directives of C# *)
%}

%token <string> Ident "<ident>"
%token True "true"
%token False "false"
%token OR "||"
%token AND "&&"
%token EQ "=="
%token NEQ "!="
%token NOT "!"
%token LPAR "("
%token RPAR ")"
%token EOL

(* Entries *)

%start expr
%type <E_AST.t> expr

%%

(* Grammar *)

expr:
  or_expr EOL { $1 }

or_expr:
  or_expr "||" and_expr         { E_AST.Or ($1,$3)  }
| and_expr                      { $1                }

and_expr:
  and_expr "&&" unary_expr      { E_AST.And ($1,$3) }
| equality_expr                 { $1                }

equality_expr:
  equality_expr "==" unary_expr { E_AST.Eq ($1,$3)  }
| equality_expr "!=" unary_expr { E_AST.Neq ($1,$3) }
| unary_expr                    { $1                }

unary_expr:
  primary_expr                  { $1                }
| "!" unary_expr                { E_AST.Not $2      }

primary_expr:
  "true"                        { E_AST.True        }
| "false"                       { E_AST.False       }
| "<ident>"                     { E_AST.Ident $1    }
| "(" or_expr ")"               { $2                }
