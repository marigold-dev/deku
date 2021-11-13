(* The API of the preprocessor

  This preprocessor recognised the following directives:

  "#define", "#elif", "#else", "#endif", "#endregion", "#error",
  "#if", "#include", "#region" and "#undef".

   Those are derived from the C# preprocessor.

   Note that unknown directives will be treated like plain text
   instead of raising an error.

   Strings and comments are only recognised in text areas that are to
   be copied. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The source configuration gathers all the information about the
   input to be preprocessed. Comments can be chose from the following
   list:
      * "(*" and "*)" for blocks and "//" for lines;
      * "/*" and "*/" for blocks and "//" for lines;
      * "/*" and "*/" for blocks and "#" for lines;
   or any combination of the above. For other markers, you need to
   modify API.mll.

   If the field [input] is [None], then [stdin] is the input source.

   If the field [offsets] is [true], then error messages will include
   horizontal offsets (a la Emacs) instead of column numbers (a la
   Vim). *)

type file_path = string
type module_name = string

type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type config = <
  block   : block_comment option;
  line    : line_comment option;
  input   : file_path option;
  offsets : bool;          (* [true] for horizontal offsets *)
  dirs    : file_path list (* Directories to search for #include files *)
>

(* In case of success, a buffer containing the preprocessed input is
   returned, together with the list of imported modules and their
   locations on the file system. In case of an error, we return the
   preprocessed buffer so far. *)

type module_deps = (file_path * module_name) list
type success     = Buffer.t * module_deps
type message     = string Region.reg

type result = (success, Buffer.t option * message) Stdlib.result

type 'src preprocessor = config -> 'src -> result

(* Preprocessing from various sources *)

val from_lexbuf  : Lexing.lexbuf preprocessor
val from_channel : in_channel    preprocessor
val from_string  : string        preprocessor
val from_file    : file_path     preprocessor
