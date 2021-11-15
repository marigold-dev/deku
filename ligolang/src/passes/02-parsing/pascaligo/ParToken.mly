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
(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <lexeme Wrap.wrap> String    "<string>" [@recover.expr wrap_ghost "<invalid-string-literal>"]
%token                  <lexeme Wrap.wrap> Verbatim  "<verbatim>" [@recover.expr wrap_ghost "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes     "<bytes>" [@recover.expr wrap_ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(lexeme * Z.t) Wrap.wrap> Int       "<int>" [@recover.expr wrap_ghost ("<invalid-int-literal>", Z.zero)]
%token          <(lexeme * Z.t) Wrap.wrap> Nat       "<nat>" [@recover.expr wrap_ghost ("<invalid-nat-literal>", Z.zero)]
%token          <(lexeme * Z.t) Wrap.wrap> Mutez     "<mutez>" [@recover.expr wrap_ghost ("<invalid-mutz-literal>", Z.zero)]
%token                  <lexeme Wrap.wrap> Ident     "<ident>" [@recover.expr wrap_ghost "<invalid-ident>"]
%token                  <lexeme Wrap.wrap> UIdent    "<uident>" [@recover.expr wrap_ghost "<invalid-uident>"]
%token                  <string Wrap.wrap> Attr      "[@attr]" [@recover.expr wrap_ghost "<invalid-attr-literal>"]
%token      <lexeme Region.reg Region.reg> Lang      "[%lang" [@recover.expr Region.wrap_ghost @@ Region.wrap_ghost "<invalid-lang-literal>"]

  (* Symbols *)

%token <lexeme Wrap.wrap> SEMI        ";" [@recover.expr wrap_ghost ";"]
%token <lexeme Wrap.wrap> COMMA       "," [@recover.expr wrap_ghost ","]
%token <lexeme Wrap.wrap> LPAR        "(" [@recover.expr wrap_ghost "("]
%token <lexeme Wrap.wrap> RPAR        ")" [@recover.expr wrap_ghost ")"]
%token <lexeme Wrap.wrap> LBRACE      "{" [@recover.expr wrap_ghost "{"]
%token <lexeme Wrap.wrap> RBRACE      "}" [@recover.expr wrap_ghost "}"]
%token <lexeme Wrap.wrap> LBRACKET    "[" [@recover.expr wrap_ghost "["]
%token <lexeme Wrap.wrap> RBRACKET    "]" [@recover.expr wrap_ghost "]"]
%token <lexeme Wrap.wrap> CONS        "#" [@recover.expr wrap_ghost "#"]
%token <lexeme Wrap.wrap> VBAR        "|" [@recover.expr wrap_ghost "|"]
%token <lexeme Wrap.wrap> ARROW       "->" [@recover.expr wrap_ghost "->"]
%token <lexeme Wrap.wrap> ASS         ":=" [@recover.expr wrap_ghost ":="]
%token <lexeme Wrap.wrap> EQ          "=" [@recover.expr wrap_ghost "="]
%token <lexeme Wrap.wrap> COLON       ":" [@recover.expr wrap_ghost ":"]
%token <lexeme Wrap.wrap> LT          "<" [@recover.expr wrap_ghost "<"]
%token <lexeme Wrap.wrap> LE          "<=" [@recover.expr wrap_ghost "<="]
%token <lexeme Wrap.wrap> GT          ">" [@recover.expr wrap_ghost ">"]
%token <lexeme Wrap.wrap> GE          ">=" [@recover.expr wrap_ghost ">="]
%token <lexeme Wrap.wrap> NE          "=/=" [@recover.expr wrap_ghost "=/="]
%token <lexeme Wrap.wrap> PLUS        "+" [@recover.expr wrap_ghost "+"]
%token <lexeme Wrap.wrap> MINUS       "-" [@recover.expr wrap_ghost "-"]
%token <lexeme Wrap.wrap> SLASH       "/" [@recover.expr wrap_ghost "/"]
%token <lexeme Wrap.wrap> TIMES       "*" [@recover.expr wrap_ghost "*"]
%token <lexeme Wrap.wrap> DOT         "." [@recover.expr wrap_ghost "."]
%token <lexeme Wrap.wrap> WILD        "_" [@recover.expr wrap_ghost "_"]
%token <lexeme Wrap.wrap> CARET       "^" [@recover.expr wrap_ghost "^"]

  (* Keywords *)

%token <lexeme Wrap.wrap> And         "and" [@recover.expr wrap_ghost "and"]
%token <lexeme Wrap.wrap> Begin       "begin" [@recover.expr wrap_ghost "begin"]
%token <lexeme Wrap.wrap> BigMap      "big_map" [@recover.expr wrap_ghost "big_map"]
%token <lexeme Wrap.wrap> Block       "block" [@recover.expr wrap_ghost "block"]
%token <lexeme Wrap.wrap> Case        "case" [@recover.expr wrap_ghost "case"]
%token <lexeme Wrap.wrap> Const       "const" [@recover.expr wrap_ghost "const"]
%token <lexeme Wrap.wrap> Contains    "contains" [@recover.expr wrap_ghost "contains"]
%token <lexeme Wrap.wrap> Else        "else" [@recover.expr wrap_ghost "else"]
%token <lexeme Wrap.wrap> End         "end" [@recover.expr wrap_ghost "end"]
%token <lexeme Wrap.wrap> For         "for" [@recover.expr wrap_ghost "for"]
%token <lexeme Wrap.wrap> Function    "function" [@recover.expr wrap_ghost "function"]
%token <lexeme Wrap.wrap> Recursive   "recursive" [@recover.expr wrap_ghost "recursive"]
%token <lexeme Wrap.wrap> From        "from" [@recover.expr wrap_ghost "from"]
%token <lexeme Wrap.wrap> If          "if" [@recover.expr wrap_ghost "if"]
%token <lexeme Wrap.wrap> In          "in" [@recover.expr wrap_ghost "in"]
%token <lexeme Wrap.wrap> Is          "is" [@recover.expr wrap_ghost "is"]
%token <lexeme Wrap.wrap> List        "list" [@recover.expr wrap_ghost "list"]
%token <lexeme Wrap.wrap> Map         "map" [@recover.expr wrap_ghost "map"]
%token <lexeme Wrap.wrap> Mod         "mod" [@recover.expr wrap_ghost "mod"]
%token <lexeme Wrap.wrap> Nil         "nil" [@recover.expr wrap_ghost "nil"]
%token <lexeme Wrap.wrap> Not         "not" [@recover.expr wrap_ghost "not"]
%token <lexeme Wrap.wrap> Of          "of" [@recover.expr wrap_ghost "of"]
%token <lexeme Wrap.wrap> Or          "or" [@recover.expr wrap_ghost "or"]
%token <lexeme Wrap.wrap> Patch       "patch" [@recover.expr wrap_ghost "patch"]
%token <lexeme Wrap.wrap> Record      "record" [@recover.expr wrap_ghost "record"]
%token <lexeme Wrap.wrap> Remove      "remove" [@recover.expr wrap_ghost "remove"]
%token <lexeme Wrap.wrap> Set         "set" [@recover.expr wrap_ghost "set"]
%token <lexeme Wrap.wrap> Skip        "skip" [@recover.expr wrap_ghost "skip"]
%token <lexeme Wrap.wrap> Step        "step" [@recover.expr wrap_ghost "step"]
%token <lexeme Wrap.wrap> Then        "then" [@recover.expr wrap_ghost "then"]
%token <lexeme Wrap.wrap> To          "to" [@recover.expr wrap_ghost "to"]
%token <lexeme Wrap.wrap> Type        "type" [@recover.expr wrap_ghost "type"]
%token <lexeme Wrap.wrap> Var         "var" [@recover.expr wrap_ghost "var"]
%token <lexeme Wrap.wrap> While       "while" [@recover.expr wrap_ghost "while"]
%token <lexeme Wrap.wrap> With        "with" [@recover.expr wrap_ghost "with"]
%token <lexeme Wrap.wrap> Module      "module" [@recover.expr wrap_ghost "module"]

  (* Virtual tokens *)

%token <lexeme Wrap.wrap> EOF [@recover.expr wrap_ghost ""]

%%
