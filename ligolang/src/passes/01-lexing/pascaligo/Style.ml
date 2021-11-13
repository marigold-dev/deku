(* Checking style of PascaLIGO based on the lexical context *)

(* Vendor dependencies *)

module Core = LexerLib.Core
module Region = Simple_utils.Region

(* Finding the next token in a list of lexical units *)

let rec next_token markup = function
  Core.Token token :: _ ->
    Some (List.rev markup, token)
| Core.Markup m :: units ->
    next_token (m::markup) units
| Core.Directive _ :: units ->
    next_token markup units
| [] -> None

let next_token = next_token []

(* Errors *)

type error =
  Odd_lengthed_bytes
| Missing_break

let error_to_string = function
  Odd_lengthed_bytes ->
    "The length of the byte sequence is an odd number.\n\
     Hint: Add or remove a digit."
| Missing_break ->
    "Missing break.\n\
     Hint: Insert some space."

let fail region error =
  let msg = error_to_string error in
  Stdlib.Error Region.{value=msg;region}

(* Predicates on the tokens *)

let is_int    = function Token.Int _ -> true | _ -> false
let is_string = function Token.String _ -> true | _ -> false
let is_bytes  = function Token.Bytes _ -> true | _ -> false

let is_hexa = function
  Token.UIdent t -> 
    if t#payload = "A" || t#payload = "a"|| t#payload = "B"|| t#payload = "b"|| t#payload = "C"|| t#payload = "c"
                 || t#payload = "D"|| t#payload = "d"|| t#payload = "E"|| t#payload = "e"|| t#payload = "F"|| t#payload = "f" then true
    else 
      false
  | _ -> false

let is_sym =
  let open Token in
  function
    SEMI _
  | COMMA _
  | LPAR _
  | RPAR _
  | LBRACE _
  | RBRACE _
  | LBRACKET _
  | RBRACKET _
  | CONS _
  | VBAR _
  | ARROW _
  | ASS _
  | EQ _
  | COLON _
  | LT _
  | LE _
  | GT _
  | GE _
  | NE _
  | PLUS _
  | MINUS _
  | SLASH _
  | TIMES _
  | DOT _
  | WILD _
  | CARET _ -> true
  | _ -> false

(* Checking the style *)

type lex_units = Token.t Core.lex_unit list

type message = string Region.reg

let rec check orig = function
  Core.Token token :: units ->
    let pos    = (Token.to_region token)#stop in
    let region = Region.make ~start:pos ~stop:pos in
    (match next_token units with
       Some ([], next) ->
         let open Token in
         if   is_int token || is_string token
         then if   is_sym next || is_eof next
              then check orig units
              else fail region Missing_break
         else
           if   is_bytes token
           then if   is_int next || is_hexa next
                then fail region Odd_lengthed_bytes
                else
                  if   is_sym next || is_eof next
                  then check orig units
                  else fail region Missing_break
           else check orig units
     | _ -> check orig units)
| (Core.Markup _ | Core.Directive _) :: units ->
     check orig units
| [] -> Ok orig

let check = function
  Stdlib.Ok units -> check units units
| Error _ as err -> err
