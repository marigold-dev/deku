(* Note: All external symbols here should be unqualified because this file is used
         by [menhir] that does not always insert the [%{..%}] header. So we work
         around it by the [-open Module] option in [dune] but symbols should be
         unqualified.

         Also, keep in mind that [ParToken.mly] and [Parser.mly] are merging into
         one file and the header of [Parser.mly] affects this code.
         For example: [lexeme] type comes from [open CST] *)

%[@recover.prelude
  open Lexing_shared.Wrap
  module Region = Simple_utils.Region
 ]
(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None))]
%token                  <string Wrap.wrap> String    "<string>" [@recover.expr wrap_ghost "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim  "<verbatim>" [@recover.expr wrap_ghost "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes     "<bytes>" [@recover.expr wrap_ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int       "<int>" [@recover.expr wrap_ghost ("<invalid-int-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.wrap> Nat       "<nat>" [@recover.expr wrap_ghost ("<invalid-nat-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.wrap> Mutez     "<mutez>" [@recover.expr wrap_ghost ("<invalid-mutez-literal>", Z.zero)]
%token                  <string Wrap.wrap> Ident     "<ident>" [@recover.expr wrap_ghost "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent    "<uident>" [@recover.expr wrap_ghost "<invalid-uident>"]
%token                  <string Wrap.wrap> Attr      "[@attr]" [@recover.expr wrap_ghost "<invalid-attr-literal>"]
%token      <lexeme Region.reg Region.reg> Lang      "[%lang" [@recover.expr Region.wrap_ghost @@ Region.wrap_ghost "<invalid-lang-literal>"] 

  (* Symbols *)

%token <lexeme Wrap.wrap> MINUS   "-" [@recover.expr wrap_ghost "-"]
%token <lexeme Wrap.wrap> PLUS    "+" [@recover.expr wrap_ghost "+"]
%token <lexeme Wrap.wrap> SLASH   "/" [@recover.expr wrap_ghost "/"]
%token <lexeme Wrap.wrap> TIMES   "*" [@recover.expr wrap_ghost "*"]

%token <lexeme Wrap.wrap> LPAR     "(" [@recover.expr wrap_ghost "("]
%token <lexeme Wrap.wrap> RPAR     ")" [@recover.expr wrap_ghost ")"]
%token <lexeme Wrap.wrap> LBRACKET "[" [@recover.expr wrap_ghost "["]
%token <lexeme Wrap.wrap> RBRACKET "]" [@recover.expr wrap_ghost "]"]
%token <lexeme Wrap.wrap> LBRACE   "{" [@recover.expr wrap_ghost "{"]
%token <lexeme Wrap.wrap> RBRACE   "}" [@recover.expr wrap_ghost "}"]

%token <lexeme Wrap.wrap> PLUS2     "++" [@recover.expr wrap_ghost "++"]
%token <lexeme Wrap.wrap> DOT       "." [@recover.expr wrap_ghost "."]
%token <lexeme Wrap.wrap> ELLIPSIS  "..." [@recover.expr wrap_ghost "..."]

%token <lexeme Wrap.wrap> COMMA "," [@recover.expr wrap_ghost ","]
%token <lexeme Wrap.wrap> SEMI  ";" [@recover.expr wrap_ghost ";"]
%token <lexeme Wrap.wrap> COLON ":" [@recover.expr wrap_ghost ":"]
%token <lexeme Wrap.wrap> VBAR  "|" [@recover.expr wrap_ghost "|"]

%token <lexeme Wrap.wrap> WILD "_" [@recover.expr wrap_ghost "_"]

%token <lexeme Wrap.wrap> EQ    "=" [@recover.expr wrap_ghost "="]
%token <lexeme Wrap.wrap> EQ2   "==" [@recover.expr wrap_ghost "=="]
%token <lexeme Wrap.wrap> NE    "!=" [@recover.expr wrap_ghost "!="]
%token <lexeme Wrap.wrap> LT    "<" [@recover.expr wrap_ghost "<"]
%token <lexeme Wrap.wrap> GT    ">" [@recover.expr wrap_ghost ">"]
%token <lexeme Wrap.wrap> LE    "<=" [@recover.expr wrap_ghost "<="]
%token <lexeme Wrap.wrap> GE    ">=" [@recover.expr wrap_ghost ">="]
%token <lexeme Wrap.wrap> ARROW "=>" [@recover.expr wrap_ghost "=>"]

%token <lexeme Wrap.wrap> NOT      "!" [@recover.expr wrap_ghost "!"]
%token <lexeme Wrap.wrap> BOOL_OR  "||" [@recover.expr wrap_ghost "||"]
%token <lexeme Wrap.wrap> BOOL_AND "&&" [@recover.expr wrap_ghost "&&"]
%token <lexeme Wrap.wrap> QUOTE    "'" [@recover.expr wrap_ghost "'"]

  (* Keywords *)

%token <lexeme Wrap.wrap> Else   "else" [@recover.expr wrap_ghost "else"]
%token <lexeme Wrap.wrap> If     "if" [@recover.expr wrap_ghost "if"]
%token <lexeme Wrap.wrap> Let    "let" [@recover.expr wrap_ghost "let"]
%token <lexeme Wrap.wrap> Rec    "rec" [@recover.expr wrap_ghost "rec"]
%token <lexeme Wrap.wrap> Switch "switch" [@recover.expr wrap_ghost "switch"]
%token <lexeme Wrap.wrap> Mod    "mod" [@recover.expr wrap_ghost "mod"]
%token <lexeme Wrap.wrap> Land   "land" [@recover.expr wrap_ghost "land"]
%token <lexeme Wrap.wrap> Lor    "lor" [@recover.expr wrap_ghost "lor"]
%token <lexeme Wrap.wrap> Lxor   "lxor" [@recover.expr wrap_ghost "lxor"]
%token <lexeme Wrap.wrap> Lsl    "lsl" [@recover.expr wrap_ghost "lsl"]
%token <lexeme Wrap.wrap> Lsr    "lsr" [@recover.expr wrap_ghost "lsr"]
%token <lexeme Wrap.wrap> Or     "or" [@recover.expr wrap_ghost "or"]
%token <lexeme Wrap.wrap> Type   "type" [@recover.expr wrap_ghost "type"]
%token <lexeme Wrap.wrap> Module "module" [@recover.expr wrap_ghost "module"]

  (* Virtual tokens *)

%token <lexeme Wrap.wrap> EOF [@recover.expr wrap_ghost ""]
%token <lexeme Wrap.wrap> ES6FUN [@recover.expr wrap_ghost ""]

%%
