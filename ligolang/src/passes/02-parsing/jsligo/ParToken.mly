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

  let default_loc = ref Region.ghost

  let mk str = wrap str !default_loc

  let mkDirective () = LexerLib.Directive.Linemarker Region.{value = (0, "<invalid-path>", None);
                                                             region = !default_loc}
]
(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"   [@recover.expr mkDirective ()]
%token                  <string Wrap.wrap> BlockCom "<block_comment>" [@recover.expr mk "<invalid-block-comment>"]
%token                  <string Wrap.wrap> LineCom  "<line_comment>"  [@recover.expr mk "<invalid-line-comment>"]
%token                  <string Wrap.wrap> String   "<string>"        [@recover.expr mk "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim "<verbatim>"      [@recover.expr mk "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes    "<bytes>"         [@recover.expr mk ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int      "<int>"           [@recover.expr mk ("<invalid-int-literal>", Z.zero)]
(* %token          <(string * Z.t) Wrap.wrap> Nat      "<nat>"*)
(* %token          <(string * Z.t) Wrap.wrap> Mutez    "<mutez>"*)
%token                  <string Wrap.wrap> Ident    "<ident>"         [@recover.expr mk "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent   "<uident>"        [@recover.expr mk "<invalid-uident>"]
%token                  <string Wrap.wrap> Attr     "[@attr]"         [@recover.expr mk "<invalid-attr-literal>"]
// %token       <lexeme Region.reg Region.reg> Lang     "[%lang"

  (* Symbols *)

%token <lexeme Wrap.wrap> MINUS   "-" [@recover.expr mk "-"]
%token <lexeme Wrap.wrap> PLUS    "+" [@recover.expr mk "+"]
%token <lexeme Wrap.wrap> SLASH   "/" [@recover.expr mk "/"]
%token <lexeme Wrap.wrap> TIMES   "*" [@recover.expr mk "*"]
%token <lexeme Wrap.wrap> REM     "%" [@recover.expr mk "%"]
(* %token <lexeme Wrap.wrap> PLUS2   "++"*)
(* %token <lexeme Wrap.wrap> MINUS2  "--"*)

%token <lexeme Wrap.wrap> LPAR     "(" [@recover.expr mk "("]
%token <lexeme Wrap.wrap> RPAR     ")" [@recover.expr mk ")"]
%token <lexeme Wrap.wrap> LBRACKET "[" [@recover.expr mk "["]
%token <lexeme Wrap.wrap> RBRACKET "]" [@recover.expr mk "]"]
%token <lexeme Wrap.wrap> LBRACE   "{" [@recover.expr mk "{"]
%token <lexeme Wrap.wrap> RBRACE   "}" [@recover.expr mk "}"]

%token <lexeme Wrap.wrap> COMMA     ","   [@recover.expr mk ","]
%token <lexeme Wrap.wrap> SEMI      ";"   [@recover.expr mk ";"]
%token <lexeme Wrap.wrap> COLON     ":"   [@recover.expr mk ":"]
%token <lexeme Wrap.wrap> DOT       "."   [@recover.expr mk "."]
%token <lexeme Wrap.wrap> ELLIPSIS  "..." [@recover.expr mk "..."]

%token <lexeme Wrap.wrap> BOOL_OR  "||" [@recover.expr mk "||"]
%token <lexeme Wrap.wrap> BOOL_AND "&&" [@recover.expr mk "&&"]
%token <lexeme Wrap.wrap> BOOL_NOT "!"  [@recover.expr mk "!"]

// %token <lexeme Wrap.wrap> BIT_AND  "&"
// %token <lexeme Wrap.wrap> BIT_NOT  "~"
// %token <lexeme Wrap.wrap> BIT_XOR  "^"
// %token <lexeme Wrap.wrap> SHIFT_L  "<<<"
// %token <lexeme Wrap.wrap> SHIFT_R  ">>>"

%token <lexeme Wrap.wrap> EQ    "="  [@recover.expr mk "="]
%token <lexeme Wrap.wrap> EQ2   "==" [@recover.expr mk "=="]
%token <lexeme Wrap.wrap> NE    "!=" [@recover.expr mk "!="]

%token <lexeme Wrap.wrap> LT    "<"  [@recover.expr mk "<"]
%token <lexeme Wrap.wrap> GT    ">"  [@recover.expr mk ">"]
%token <lexeme Wrap.wrap> LE    "<=" [@recover.expr mk "<="]
%token <lexeme Wrap.wrap> GE    ">=" [@recover.expr mk ">="]

%token <lexeme Wrap.wrap> PLUS_EQ  "+=" [@recover.expr mk "+="]
%token <lexeme Wrap.wrap> MINUS_EQ "-=" [@recover.expr mk "-="]
%token <lexeme Wrap.wrap> MULT_EQ  "*=" [@recover.expr mk "*="]
%token <lexeme Wrap.wrap> REM_EQ   "%=" [@recover.expr mk "%="]
%token <lexeme Wrap.wrap> DIV_EQ   "/=" [@recover.expr mk "/="]
// %token <lexeme Wrap.wrap> SL_EQ    "<<<="
// %token <lexeme Wrap.wrap> SR_EQ    ">>>="
// %token <lexeme Wrap.wrap> AND_EQ   "&="
// %token <lexeme Wrap.wrap> OR_EQ    "|="
// %token <lexeme Wrap.wrap> XOR_EQ   "^="

%token <lexeme Wrap.wrap> VBAR   "|"  [@recover.expr mk "|"]
%token <lexeme Wrap.wrap> ARROW  "=>" [@recover.expr mk "=>"]
%token <lexeme Wrap.wrap> WILD   "_"  [@recover.expr mk "_"]


(* JavaScript Keywords *)

%token <lexeme Wrap.wrap> Case     "case"    [@recover.expr mk "case"]
%token <lexeme Wrap.wrap> Const    "const"   [@recover.expr mk "const"]
%token <lexeme Wrap.wrap> Default  "default" [@recover.expr mk "default"]
%token <lexeme Wrap.wrap> Else     "else"    [@recover.expr mk "else"]
%token <lexeme Wrap.wrap> Export   "export"  [@recover.expr mk "export"]
%token <lexeme Wrap.wrap> For      "for"     [@recover.expr mk "for"]
%token <lexeme Wrap.wrap> If       "if"      [@recover.expr mk "if"]
%token <lexeme Wrap.wrap> Import   "import"  [@recover.expr mk "import"]
%token <lexeme Wrap.wrap> Let      "let"     [@recover.expr mk "let"]
%token <lexeme Wrap.wrap> Of       "of"      [@recover.expr mk "of"]
%token <lexeme Wrap.wrap> Return   "return"  [@recover.expr mk "return"]
%token <lexeme Wrap.wrap> Break    "break"   [@recover.expr mk "break"]
%token <lexeme Wrap.wrap> Switch   "switch"  [@recover.expr mk "switch"]
%token <lexeme Wrap.wrap> While    "while"   [@recover.expr mk "while"]

(* TypeScript keywords *)

%token <lexeme Wrap.wrap> As        "as"        [@recover.expr mk "as"]
%token <lexeme Wrap.wrap> Namespace "namespace" [@recover.expr mk "namespace"]
%token <lexeme Wrap.wrap> Type      "type"      [@recover.expr mk "type"]

(* Virtual tokens *)

%token <lexeme Wrap.wrap> ZWSP [@recover.expr mk ""]

(* End of File *)

%token <lexeme Wrap.wrap> EOF [@recover.expr mk ""]

%%
