(* Definition of preprocessing directives for the lexer *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* A preprocessing directive is made of an '#' followed by a
   predefined name.

   Line markers are one of such directive, which may carry some
   additional flags:

   https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html

   of which 1 and 2 indicate, respectively, the start of a new file
   and the return from a file (after its inclusion has been
   processed). *)

type linenum    = int
type file_path  = string
type flag       = Push | Pop
type linemarker = linenum * file_path * flag option

type t =
  Linemarker of linemarker Region.reg

type directive = t

(* Printing *)

type lexeme = string

val to_lexeme : t -> lexeme
val to_string : offsets:bool -> [`Byte | `Point] -> t -> string
val project   : t -> Region.t * string
val to_region : t -> Region.t
