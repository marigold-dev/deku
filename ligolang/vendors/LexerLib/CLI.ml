(* Parsing command-line options for the lexer *)

(* Vendor dependencies *)

module Argv = Simple_utils.Argv

(* Preprocessor CLI *)

module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end

module type PREPROCESSOR_CLI =
  sig
    include COMMENTS

    val input     : string option (* input file     *)
    val extension : string option (* file extension *)
    val dirs      : string list   (* -I             *)
    val show_pp   : bool          (* --show-pp      *)
    val offsets   : bool          (* neg --columns  *)

    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    val status : status
  end

(* The signature [S] (command-line interface) gathers the options
   given to the tool, following the GNu convention, and exports then
   as module fields. *)

module type S =
  sig
    module Preprocessor_CLI : PREPROCESSOR_CLI

    val preprocess : bool
    val mode       : [`Byte | `Point]
    val command    : [`Copy | `Units | `Tokens] option

    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string
    ]

    val status : status
  end

(* Parsing the command line options *)

module Make (Preprocessor_CLI: PREPROCESSOR_CLI) : S =
  struct
    module Preprocessor_CLI = Preprocessor_CLI

    (* Auxiliary functions and modules *)

    let sprintf = Printf.sprintf

    (* Help (exported) *)

    let make_help buffer : Buffer.t =
      let options = [
        "  -t, --tokens     Print tokens";
        "  -u, --units      Print lexical units";
        "  -c, --copy       Print lexemes and markup";
        "      --bytes      Bytes for source locations";
        "      --preprocess Run the preprocessor"
      ] in
      begin
        Buffer.add_string buffer (String.concat "\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* Specifying the command-line options a la GNU *)

    let copy       = ref false
    and tokens     = ref false
    and units      = ref false
    and bytes      = ref false
    and preprocess = ref false

    and help       = ref false
    and version    = ref false
    and cli        = ref false

    (* The following has been copied and pasted from the
       implementation of the module [Getopt], under the original
       licence, namely:

       Copyright (C) 2000-2004 Alain Frisch. Distributed under the
       terms of the MIT license.

       Layout of the command line

         There are two types of argument on the command line: options
         and anonymous arguments. Options may have two forms: a short
         one introduced by a single dash character (-) and a long one
         introduced by a double dash (--).

         Options may have an argument attached. For the long form, the
         syntax is "--option=argument". For the short form, there are
         two possible syntaxes: "-o argument" (argument doesn't start
         with a dash) and "-oargument"

         Short options that refuse arguments may be concatenated, as in
         "-opq".

         The special argument -- interrupts the parsing of options:
         all the remaining arguments are arguments even they start
         with a dash.

       Command line specification

         A specification lists the possible options and describe what
         to do when they are found; it also gives the action for
         anonymous arguments and for the special option - (a single
         dash alone).

         The specification for a single option is a tuple
         [(short_form, long_form, action, handler)] where:

         - [short_form] is the character for the short form of the
           option without the leading - (or [noshort='\000'] if the
           option does not have a short form)

         - [long_form] is the string for the long form of the option
           without the leading -- (or [nolong=""] if no long form)

         - [(action : (unit -> unit) option)] gives the action to be
           executed when the option is found without an argument

         - [(handler : (string -> unit) option)] specifies how to
           handle the argument when the option is found with the
           argument

         According to the pair [(action, handler)], the corresponding
         option may, must or mustn't have an argument :

         - [(Some _, Some _)]: the option may have an argument; the
           short form can't be concatenated with other options (even
           if the user does not want to provide an argument). The
           behaviour (handler/action) is determined by the presence of
           the argument.

         - [(Some _, None)]: the option must not have an argument; the
           short form, if it exists, may be concatenated

         - [(None, Some _)]: the option must have an argument; the
           short form can't be concatenated

         - [(None, None)]: not allowed *)

    let specs =
      let open! Getopt in [
        noshort, "copy",       set copy true, None;
        noshort, "tokens",     set tokens true, None;
        noshort, "units",      set units true, None;
        noshort, "bytes",      set bytes true, None;
        noshort, "preprocess", set preprocess true, None;

        noshort, "cli",        set cli true, None;
        'h',     "help",       set help true, None;
        'v',     "version",    set version true, None
        ]

     (* Handler of anonymous arguments: those have been handled by a
        previous IO *)

    let anonymous _arg = ()

    (* Parsing the command-line options *)

    (* We do not want the exception [Getopt.Error] to be raised when
       finding an unknown option.

       The following is a hack to filter out unknown options but
       leaving correct ones, even if their syntax is invalid (this
       will result in an error in [Getopt.parse_cmdline] below. Also,
       we assume that there are no concatenated short options (here,
       the only possible combinations are "-hv" and "-vh") and that
       anonymous arguments (here, a unique text file) is given after
       "--".

       We make a copy of [Sys.argv], we filter it in a list, the
       resulting list is copied to [Sys.argv] (with the remaning cells
       set to [""]), we parse the options with [Getopt.parse_cmdline]
       and we finally restore [Sys.argv] from its original copy.

       Before parsing the command-line, we assign the status with the
       status of the previous CLI (here,
       [Preprocessor_CLI.status]). *)

    module SSet = Set.Make (String)

    let opt_wo_arg =
      let open SSet in
      empty
      |> add "--copy"
      |> add "--tokens"
      |> add "--units"
      |> add "--preprocess"

      (* The following options are present in all CLI *)
      |> add "--cli"
      |> add "--help" |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg = SSet.empty

    let argv_copy = Array.copy Sys.argv

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg

    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string
    ]

    let status = (Preprocessor_CLI.status :> status)

    let status =
      try
        Getopt.parse_cmdline specs anonymous; status
      with Getopt.Error msg -> `SyntaxError msg

    let () =
      for i = 0 to Array.length Sys.argv - 1 do
        Sys.argv.(i) <- argv_copy.(i)
      done

    (* Re-exporting immutable fields with their CLI value *)

    let copy       = !copy
    and tokens     = !tokens
    and units      = !units
    and mode       = if !bytes then `Byte else `Point
    and preprocess = !preprocess

    (* Re-exporting and printing on stdout the CLI options *)

    let make_cli buffer : Buffer.t =
      (* Options "help", "version" and "cli" are not given. *)
      let options = [
        sprintf "copy       = %b" copy;
        sprintf "tokens     = %b" tokens;
        sprintf "units      = %b" units;
        sprintf "bytes      = %b" !bytes;
        sprintf "preprocess = %b" preprocess] in
    begin
      Buffer.add_string buffer (String.concat "\n" options);
      Buffer.add_char   buffer '\n';
      buffer
    end

    (* Checking combinations of options *)

    let status, command =
      match copy, units, tokens with
        true, false, false -> status, Some `Copy
      | false,  true, false -> status, Some `Units
      | false, false,  true -> status, Some `Tokens
      | false, false, false -> status, None
      | true, true, _ -> `Conflict ("--copy", "--units"), None
      | true, _, true -> `Conflict ("--copy", "--tokens"), None
      | _, true, true -> `Conflict ("--units", "--tokens"), None

    (* Status *)

    let status =
      match status with
        `Help buffer  -> `Help (make_help buffer)
      | `CLI buffer   -> `CLI (make_cli buffer)
      | `Version _    -> `Version Version.version
      | _             -> status

  end
