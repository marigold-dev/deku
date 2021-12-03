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
(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr mkDirective () ]
%token                  <lexeme Wrap.wrap> String    "<string>"     [@recover.expr mk "<invalid-string-literal>"]
%token                  <lexeme Wrap.wrap> Verbatim  "<verbatim>"   [@recover.expr mk "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes     "<bytes>"      [@recover.expr mk ("<invalid-bytes-literal>", `Hex "")]
%token          <(lexeme * Z.t) Wrap.wrap> Int       "<int>"        [@recover.expr mk ("<invalid-int-literal>", Z.zero)]
%token          <(lexeme * Z.t) Wrap.wrap> Nat       "<nat>"        [@recover.expr mk ("<invalid-nat-literal>", Z.zero)]
%token          <(lexeme * Z.t) Wrap.wrap> Mutez     "<mutez>"      [@recover.expr mk ("<invalid-mutz-literal>", Z.zero)]
%token                  <lexeme Wrap.wrap> Ident     "<ident>"      [@recover.expr mk "<invalid-ident>"]
%token                  <lexeme Wrap.wrap> UIdent    "<uident>"     [@recover.expr mk "<invalid-uident>"]
%token                  <string Wrap.wrap> Attr      "[@attr]"      [@recover.expr mk "<invalid-attr-literal>"]
%token      <lexeme Region.reg Region.reg> Lang      "[%lang"       [@recover.expr mkLang ()]

  (* Symbols *)

%token <lexeme Wrap.wrap> SEMI        ";"   [@recover.expr mk ";"]
%token <lexeme Wrap.wrap> COMMA       ","   [@recover.expr mk ","]
%token <lexeme Wrap.wrap> LPAR        "("   [@recover.expr mk "("]
%token <lexeme Wrap.wrap> RPAR        ")"   [@recover.expr mk ")"]
%token <lexeme Wrap.wrap> LBRACE      "{"   [@recover.expr mk "{"]
%token <lexeme Wrap.wrap> RBRACE      "}"   [@recover.expr mk "}"]
%token <lexeme Wrap.wrap> LBRACKET    "["   [@recover.expr mk "["]
%token <lexeme Wrap.wrap> RBRACKET    "]"   [@recover.expr mk "]"]
%token <lexeme Wrap.wrap> CONS        "#"   [@recover.expr mk "#"]
%token <lexeme Wrap.wrap> VBAR        "|"   [@recover.expr mk "|"]
%token <lexeme Wrap.wrap> ARROW       "->"  [@recover.expr mk "->"]
%token <lexeme Wrap.wrap> ASS         ":="  [@recover.expr mk ":="]
%token <lexeme Wrap.wrap> EQ          "="   [@recover.expr mk "="]
%token <lexeme Wrap.wrap> COLON       ":"   [@recover.expr mk ":"]
%token <lexeme Wrap.wrap> LT          "<"   [@recover.expr mk "<"]
%token <lexeme Wrap.wrap> LE          "<="  [@recover.expr mk "<="]
%token <lexeme Wrap.wrap> GT          ">"   [@recover.expr mk ">"]
%token <lexeme Wrap.wrap> GE          ">="  [@recover.expr mk ">="]
%token <lexeme Wrap.wrap> NE          "=/=" [@recover.expr mk "=/="]
%token <lexeme Wrap.wrap> PLUS        "+"   [@recover.expr mk "+"]
%token <lexeme Wrap.wrap> MINUS       "-"   [@recover.expr mk "-"]
%token <lexeme Wrap.wrap> SLASH       "/"   [@recover.expr mk "/"]
%token <lexeme Wrap.wrap> TIMES       "*"   [@recover.expr mk "*"]
%token <lexeme Wrap.wrap> DOT         "."   [@recover.expr mk "."]
%token <lexeme Wrap.wrap> WILD        "_"   [@recover.expr mk "_"]
%token <lexeme Wrap.wrap> CARET       "^"   [@recover.expr mk "^"]

  (* Keywords *)

%token <lexeme Wrap.wrap> And         "and"       [@recover.expr mk "and"]
%token <lexeme Wrap.wrap> Begin       "begin"     [@recover.expr mk "begin"]
%token <lexeme Wrap.wrap> BigMap      "big_map"   [@recover.expr mk "big_map"]
%token <lexeme Wrap.wrap> Block       "block"     [@recover.expr mk "block"]
%token <lexeme Wrap.wrap> Case        "case"      [@recover.expr mk "case"]
%token <lexeme Wrap.wrap> Const       "const"     [@recover.expr mk "const"]
%token <lexeme Wrap.wrap> Contains    "contains"  [@recover.expr mk "contains"]
%token <lexeme Wrap.wrap> Else        "else"      [@recover.expr mk "else"]
%token <lexeme Wrap.wrap> End         "end"       [@recover.expr mk "end"]
%token <lexeme Wrap.wrap> For         "for"       [@recover.expr mk "for"]
%token <lexeme Wrap.wrap> Function    "function"  [@recover.expr mk "function"]
%token <lexeme Wrap.wrap> Recursive   "recursive" [@recover.expr mk "recursive"]
%token <lexeme Wrap.wrap> From        "from"      [@recover.expr mk "from"]
%token <lexeme Wrap.wrap> If          "if"        [@recover.expr mk "if"]
%token <lexeme Wrap.wrap> In          "in"        [@recover.expr mk "in"]
%token <lexeme Wrap.wrap> Is          "is"        [@recover.expr mk "is"]
%token <lexeme Wrap.wrap> List        "list"      [@recover.expr mk "list"]
%token <lexeme Wrap.wrap> Map         "map"       [@recover.expr mk "map"]
%token <lexeme Wrap.wrap> Mod         "mod"       [@recover.expr mk "mod"]
%token <lexeme Wrap.wrap> Nil         "nil"       [@recover.expr mk "nil"]
%token <lexeme Wrap.wrap> Not         "not"       [@recover.expr mk "not"]
%token <lexeme Wrap.wrap> Of          "of"        [@recover.expr mk "of"]
%token <lexeme Wrap.wrap> Or          "or"        [@recover.expr mk "or"]
%token <lexeme Wrap.wrap> Patch       "patch"     [@recover.expr mk "patch"]
%token <lexeme Wrap.wrap> Record      "record"    [@recover.expr mk "record"]
%token <lexeme Wrap.wrap> Remove      "remove"    [@recover.expr mk "remove"]
%token <lexeme Wrap.wrap> Set         "set"       [@recover.expr mk "set"]
%token <lexeme Wrap.wrap> Skip        "skip"      [@recover.expr mk "skip"]
%token <lexeme Wrap.wrap> Step        "step"      [@recover.expr mk "step"]
%token <lexeme Wrap.wrap> Then        "then"      [@recover.expr mk "then"]
%token <lexeme Wrap.wrap> To          "to"        [@recover.expr mk "to"]
%token <lexeme Wrap.wrap> Type        "type"      [@recover.expr mk "type"]
%token <lexeme Wrap.wrap> Var         "var"       [@recover.expr mk "var"]
%token <lexeme Wrap.wrap> While       "while"     [@recover.expr mk "while"]
%token <lexeme Wrap.wrap> With        "with"      [@recover.expr mk "with"]
%token <lexeme Wrap.wrap> Module      "module"    [@recover.expr mk "module"]

  (* Virtual tokens *)

%token <lexeme Wrap.wrap> EOF [@recover.expr mk ""]

%%
