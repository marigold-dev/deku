(* Interfacing the preprocessor. *)

(* CONFIGURATION *)

type file_path = string
type dirs = file_path list (* #include and #import *)

module type FILE =
  sig
    include File.S
    val input : file_path option
    val dirs  : dirs
  end

module Config (File : FILE) (Comments : Comments.S) =
  struct
    (* Stubs for the libraries CLIs *)

    module Preprocessor_CLI : Preprocessor.CLI.S =
      struct
        include Comments

        let input     = File.input
        let extension = Some File.extension
        let dirs      = File.dirs
        let show_pp   = false
        let offsets   = true  (* TODO: Should flow from CLI *)

        type status = [
          `Done
        | `Version      of string
        | `Help         of Buffer.t
        | `CLI          of Buffer.t
        | `SyntaxError  of string
        | `FileNotFound of string
        ]

        let status = `Done
      end

    (* Configurations for the preprocessor based on the
       librairies CLIs. *)

    let preprocessor =
      object
        method block   = Preprocessor_CLI.block
        method line    = Preprocessor_CLI.line
        method input   = Preprocessor_CLI.input
        method offsets = Preprocessor_CLI.offsets
        method dirs    = Preprocessor_CLI.dirs
      end
  end

(* PREPROCESSING *)

module Make (File : File.S) (Comments : Comments.S) =
  struct
    (* Directories and files *)

    type nonrec file_path = file_path
    type nonrec dirs = dirs

    (* Results *)

    module Errors = Errors

    type success = Preprocessor.API.success
    type nonrec result  = (success, Errors.t) result

    (* Postlude *)

    let finalise show_pp = function
      Error (_, msg) ->
        Error (Errors.generic msg)
    | Ok (buffer, deps) ->
        let string = Buffer.contents buffer in
        if show_pp then
          Printf.printf "%s\n%!" string;
        Ok (buffer, deps)

    (* Preprocessing a file *)

    let from_file dirs file_path =
      let module File : FILE =
        struct
          let extension = File.extension
          let input     = Some file_path
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let config = Config.preprocessor in
      let preprocessed =
        Preprocessor.API.from_file config file_path in
      finalise Config.Preprocessor_CLI.show_pp preprocessed

    let preprocess_file = from_file

    (* Preprocessing a string *)

    let from_string dirs string =
      let module File : FILE =
        struct
          let extension = File.extension
          let input     = None
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let config = Config.preprocessor in
      let preprocessed =
        Preprocessor.API.from_string config string in
      finalise Config.Preprocessor_CLI.show_pp preprocessed

    let preprocess_string = from_string

    (* Preprocessing a channel *)

    let from_channel dirs channel =
      let module File : FILE =
        struct
          let extension = File.extension
          let input     = None
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let config = Config.preprocessor in
      let preprocessed =
        Preprocessor.API.from_channel config channel in
      finalise Config.Preprocessor_CLI.show_pp preprocessed

    let preprocess_channel = from_channel
  end
