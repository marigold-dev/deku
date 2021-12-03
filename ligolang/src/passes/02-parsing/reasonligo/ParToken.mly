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
  let mkLang () = Region.{value = Region.{value = "<invalid-lang-literal>";
                                          region = !default_loc};
                          region = !default_loc}
 ]
(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr mkDirective ()]
%token                  <string Wrap.wrap> String    "<string>"     [@recover.expr mk "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim  "<verbatim>"   [@recover.expr mk "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes     "<bytes>"      [@recover.expr mk ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int       "<int>"        [@recover.expr mk ("<invalid-int-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.wrap> Nat       "<nat>"        [@recover.expr mk ("<invalid-nat-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.wrap> Mutez     "<mutez>"      [@recover.expr mk ("<invalid-mutez-literal>", Z.zero)]
%token                  <string Wrap.wrap> Ident     "<ident>"      [@recover.expr mk "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent    "<uident>"     [@recover.expr mk "<invalid-uident>"]
%token                  <string Wrap.wrap> Attr      "[@attr]"      [@recover.expr mk "<invalid-attr-literal>"]
%token      <lexeme Region.reg Region.reg> Lang      "[%lang"       [@recover.expr mkLang ()]

  (* Symbols *)

%token <lexeme Wrap.wrap> MINUS   "-" [@recover.expr mk "-"]
%token <lexeme Wrap.wrap> PLUS    "+" [@recover.expr mk "+"]
%token <lexeme Wrap.wrap> SLASH   "/" [@recover.expr mk "/"]
%token <lexeme Wrap.wrap> TIMES   "*" [@recover.expr mk "*"]

%token <lexeme Wrap.wrap> LPAR     "(" [@recover.expr mk "("]
%token <lexeme Wrap.wrap> RPAR     ")" [@recover.expr mk ")"]
%token <lexeme Wrap.wrap> LBRACKET "[" [@recover.expr mk "["]
%token <lexeme Wrap.wrap> RBRACKET "]" [@recover.expr mk "]"]
%token <lexeme Wrap.wrap> LBRACE   "{" [@recover.expr mk "{"]
%token <lexeme Wrap.wrap> RBRACE   "}" [@recover.expr mk "}"]

%token <lexeme Wrap.wrap> PLUS2     "++"  [@recover.expr mk "++"]
%token <lexeme Wrap.wrap> DOT       "."   [@recover.expr mk "."]
%token <lexeme Wrap.wrap> ELLIPSIS  "..." [@recover.expr mk "..."]

%token <lexeme Wrap.wrap> COMMA "," [@recover.expr mk ","]
%token <lexeme Wrap.wrap> SEMI  ";" [@recover.expr mk ";"]
%token <lexeme Wrap.wrap> COLON ":" [@recover.expr mk ":"]
%token <lexeme Wrap.wrap> VBAR  "|" [@recover.expr mk "|"]

%token <lexeme Wrap.wrap> WILD "_"  [@recover.expr mk "_"]

%token <lexeme Wrap.wrap> EQ    "="  [@recover.expr mk "="]
%token <lexeme Wrap.wrap> EQ2   "==" [@recover.expr mk "=="]
%token <lexeme Wrap.wrap> NE    "!=" [@recover.expr mk "!="]
%token <lexeme Wrap.wrap> LT    "<"  [@recover.expr mk "<"]
%token <lexeme Wrap.wrap> GT    ">"  [@recover.expr mk ">"]
%token <lexeme Wrap.wrap> LE    "<=" [@recover.expr mk "<="]
%token <lexeme Wrap.wrap> GE    ">=" [@recover.expr mk ">="]
%token <lexeme Wrap.wrap> ARROW "=>" [@recover.expr mk "=>"]

%token <lexeme Wrap.wrap> NOT      "!"  [@recover.expr mk "!"]
%token <lexeme Wrap.wrap> BOOL_OR  "||" [@recover.expr mk "||"]
%token <lexeme Wrap.wrap> BOOL_AND "&&" [@recover.expr mk "&&"]
%token <lexeme Wrap.wrap> QUOTE    "'"  [@recover.expr mk "'"]

  (* Keywords *)

%token <lexeme Wrap.wrap> Else   "else"    [@recover.expr mk "else"]
%token <lexeme Wrap.wrap> If     "if"      [@recover.expr mk "if"]
%token <lexeme Wrap.wrap> Let    "let"     [@recover.expr mk "let"]
%token <lexeme Wrap.wrap> Rec    "rec"     [@recover.expr mk "rec"]
%token <lexeme Wrap.wrap> Switch "switch"  [@recover.expr mk "switch"]
%token <lexeme Wrap.wrap> Mod    "mod"     [@recover.expr mk "mod"]
%token <lexeme Wrap.wrap> Land   "land"    [@recover.expr mk "land"]
%token <lexeme Wrap.wrap> Lor    "lor"     [@recover.expr mk "lor"]
%token <lexeme Wrap.wrap> Lxor   "lxor"    [@recover.expr mk "lxor"]
%token <lexeme Wrap.wrap> Lsl    "lsl"     [@recover.expr mk "lsl"]
%token <lexeme Wrap.wrap> Lsr    "lsr"     [@recover.expr mk "lsr"]
%token <lexeme Wrap.wrap> Or     "or"      [@recover.expr mk "or"]
%token <lexeme Wrap.wrap> Type   "type"    [@recover.expr mk "type"]
%token <lexeme Wrap.wrap> Module "module"  [@recover.expr mk "module"]

  (* Virtual tokens *)

%token <lexeme Wrap.wrap> EOF    [@recover.expr mk ""]
%token <lexeme Wrap.wrap> ES6FUN [@recover.expr mk ""]

%%
