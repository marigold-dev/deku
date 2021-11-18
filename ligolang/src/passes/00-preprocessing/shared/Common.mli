(* PREPROCESSING *)

(* Directories and files *)

type file_path = string
type dirs = file_path list (* #include and #import *)

module Make (File : File.S) (Comments : Comments.S) :
  sig
    (* Directories and files *)

    type nonrec file_path = file_path
    type nonrec dirs = dirs

    (* Results *)

    module Errors = Errors

    type success = Preprocessor.API.success
    type nonrec result  = (success, Errors.t) result

    (* Preprocessing various sources *)

    val from_file    : dirs -> file_path  -> result
    val from_string  : dirs -> string     -> result
    val from_channel : dirs -> in_channel -> result

    (* Aliases *)

    val preprocess_file    : dirs -> file_path  -> result
    val preprocess_string  : dirs -> string     -> result
    val preprocess_channel : dirs -> in_channel -> result
  end

(* For further passes *)

module type FILE =
  sig
    include File.S
    val input : file_path option
    val dirs  : dirs
  end
