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

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <string Wrap.wrap> BlockCom "<block_comment>" [@recover.expr wrap_ghost "<invalid-block-comment>"]
%token                  <string Wrap.wrap> LineCom  "<line_comment>" [@recover.expr wrap_ghost "<invalid-line-comment>"]
%token                  <string Wrap.wrap> String   "<string>" [@recover.expr wrap_ghost "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim "<verbatim>" [@recover.expr wrap_ghost "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes    "<bytes>" [@recover.expr wrap_ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int      "<int>" [@recover.expr wrap_ghost ("<invalid-int-literal>", Z.zero)]
(* %token          <(string * Z.t) Wrap.wrap> Nat      "<nat>"*)
(* %token          <(string * Z.t) Wrap.wrap> Mutez    "<mutez>"*)
%token                  <string Wrap.wrap> Ident    "<ident>" [@recover.expr wrap_ghost "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent   "<uident>" [@recover.expr wrap_ghost "<invalid-uident>"]
%token                  <string Wrap.wrap> Attr     "[@attr]" [@recover.expr wrap_ghost "<invalid-attr-literal>"]
// %token       <lexeme Region.reg Region.reg> Lang     "[%lang"

  (* Symbols *)

%token <lexeme Wrap.wrap> MINUS   "-" [@recover.expr wrap_ghost "-"]
%token <lexeme Wrap.wrap> PLUS    "+" [@recover.expr wrap_ghost "+"]
%token <lexeme Wrap.wrap> SLASH   "/" [@recover.expr wrap_ghost "/"]
%token <lexeme Wrap.wrap> TIMES   "*" [@recover.expr wrap_ghost "*"]
%token <lexeme Wrap.wrap> REM     "%" [@recover.expr wrap_ghost "%"]
(* %token <lexeme Wrap.wrap> PLUS2   "++"*)
(* %token <lexeme Wrap.wrap> MINUS2  "--"*)

%token <lexeme Wrap.wrap> LPAR     "(" [@recover.expr wrap_ghost "("]
%token <lexeme Wrap.wrap> RPAR     ")" [@recover.expr wrap_ghost ")"]
%token <lexeme Wrap.wrap> LBRACKET "[" [@recover.expr wrap_ghost "["]
%token <lexeme Wrap.wrap> RBRACKET "]" [@recover.expr wrap_ghost "]"]
%token <lexeme Wrap.wrap> LBRACE   "{" [@recover.expr wrap_ghost "{"]
%token <lexeme Wrap.wrap> RBRACE   "}" [@recover.expr wrap_ghost "}"]

%token <lexeme Wrap.wrap> COMMA     "," [@recover.expr wrap_ghost ","]
%token <lexeme Wrap.wrap> SEMI      ";" [@recover.expr wrap_ghost ";"]
%token <lexeme Wrap.wrap> COLON     ":" [@recover.expr wrap_ghost ":"]
%token <lexeme Wrap.wrap> DOT       "." [@recover.expr wrap_ghost "."]
%token <lexeme Wrap.wrap> ELLIPSIS  "..." [@recover.expr wrap_ghost "..."]

%token <lexeme Wrap.wrap> BOOL_OR  "||" [@recover.expr wrap_ghost "||"]
%token <lexeme Wrap.wrap> BOOL_AND "&&" [@recover.expr wrap_ghost "&&"]
%token <lexeme Wrap.wrap> BOOL_NOT "!" [@recover.expr wrap_ghost "!"]

// %token <lexeme Wrap.wrap> BIT_AND  "&"
// %token <lexeme Wrap.wrap> BIT_NOT  "~"
// %token <lexeme Wrap.wrap> BIT_XOR  "^"
// %token <lexeme Wrap.wrap> SHIFT_L  "<<<"
// %token <lexeme Wrap.wrap> SHIFT_R  ">>>"

%token <lexeme Wrap.wrap> EQ    "=" [@recover.expr wrap_ghost "="]
%token <lexeme Wrap.wrap> EQ2   "==" [@recover.expr wrap_ghost "=="]
%token <lexeme Wrap.wrap> NE    "!=" [@recover.expr wrap_ghost "!="]

%token <lexeme Wrap.wrap> LT    "<" [@recover.expr wrap_ghost "<"]
%token <lexeme Wrap.wrap> GT    ">" [@recover.expr wrap_ghost ">"]
%token <lexeme Wrap.wrap> LE    "<=" [@recover.expr wrap_ghost "<="]
%token <lexeme Wrap.wrap> GE    ">=" [@recover.expr wrap_ghost ">="]

%token <lexeme Wrap.wrap> PLUS_EQ  "+=" [@recover.expr wrap_ghost "+="]
%token <lexeme Wrap.wrap> MINUS_EQ "-=" [@recover.expr wrap_ghost "-="]
%token <lexeme Wrap.wrap> MULT_EQ  "*=" [@recover.expr wrap_ghost "*="]
%token <lexeme Wrap.wrap> REM_EQ   "%=" [@recover.expr wrap_ghost "%="]
%token <lexeme Wrap.wrap> DIV_EQ   "/=" [@recover.expr wrap_ghost "/="]
// %token <lexeme Wrap.wrap> SL_EQ    "<<<="
// %token <lexeme Wrap.wrap> SR_EQ    ">>>="
// %token <lexeme Wrap.wrap> AND_EQ   "&="
// %token <lexeme Wrap.wrap> OR_EQ    "|="
// %token <lexeme Wrap.wrap> XOR_EQ   "^="

%token <lexeme Wrap.wrap> VBAR   "|" [@recover.expr wrap_ghost "|"]
%token <lexeme Wrap.wrap> ARROW  "=>" [@recover.expr wrap_ghost "=>"]
%token <lexeme Wrap.wrap> WILD   "_" [@recover.expr wrap_ghost "_"]


(* JavaScript Keywords *)

%token <lexeme Wrap.wrap> Case     "case" [@recover.expr wrap_ghost "case"]
%token <lexeme Wrap.wrap> Const    "const" [@recover.expr wrap_ghost "const"]
%token <lexeme Wrap.wrap> Default  "default" [@recover.expr wrap_ghost "default"]
%token <lexeme Wrap.wrap> Else     "else" [@recover.expr wrap_ghost "else"]
%token <lexeme Wrap.wrap> Export   "export" [@recover.expr wrap_ghost "export"]
%token <lexeme Wrap.wrap> For      "for" [@recover.expr wrap_ghost "for"]
%token <lexeme Wrap.wrap> If       "if" [@recover.expr wrap_ghost "if"]
%token <lexeme Wrap.wrap> Import   "import" [@recover.expr wrap_ghost "import"]
%token <lexeme Wrap.wrap> Let      "let" [@recover.expr wrap_ghost "let"]
%token <lexeme Wrap.wrap> Of       "of" [@recover.expr wrap_ghost "of"]
%token <lexeme Wrap.wrap> Return   "return" [@recover.expr wrap_ghost "return"]
%token <lexeme Wrap.wrap> Break    "break"  [@recover.expr wrap_ghost "break"]
%token <lexeme Wrap.wrap> Switch   "switch" [@recover.expr wrap_ghost "switch"]
%token <lexeme Wrap.wrap> While    "while" [@recover.expr wrap_ghost "while"]

(* TypeScript keywords *)

%token <lexeme Wrap.wrap> As        "as" [@recover.expr wrap_ghost "as"]
%token <lexeme Wrap.wrap> Namespace "namespace" [@recover.expr wrap_ghost "namespace"]
%token <lexeme Wrap.wrap> Type      "type" [@recover.expr wrap_ghost "type"]

(* Virtual tokens *)

%token <lexeme Wrap.wrap> ZWSP [@recover.expr wrap_ghost ""]

(* End of File *)

%token <lexeme Wrap.wrap> EOF [@recover.expr wrap_ghost ""]

%%
