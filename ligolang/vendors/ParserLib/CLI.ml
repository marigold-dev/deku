(* Parsing command-line options *)

(* Vendor dependencies *)

module Argv = Simple_utils.Argv

(* Comments *)

module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end

(* Preprocessor CLI *)

module type PREPROCESSING_CLI =
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

(* Lexer CLI *)

module type LEXER_CLI =
  sig
    module Preprocessor_CLI : PREPROCESSING_CLI

    val preprocess : bool
    val mode       : [`Byte | `Point]
    val command    : [`Copy | `Units | `Tokens] option

    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string
    ]

    val status : status
  end

(* Parser CLI *)

module type S =
  sig
    module Lexer_CLI : LEXER_CLI

    val mono       : bool
    val pretty     : bool
    val cst        : bool
    val cst_tokens : bool
    val recovery   : bool
    (* debug options *)
    val trace_recovery        : bool
    val trace_recovery_output : string option

    type status = [
      Lexer_CLI.status
    | `DependsOnOtherOption of string * string
    ]

    val status : status
  end

(* Parsing the command line options *)

module Make (Lexer_CLI: LEXER_CLI) : S =
  struct
    module Lexer_CLI = Lexer_CLI

    (* Auxiliary functions *)

    let sprintf = Printf.sprintf

    (* Help (exported) *)

    let make_help buffer : Buffer.t =
      let options = [
        "      --mono       Use Menhir monolithic API";
        "      --cst        Print the CST";
        "      --cst-tokens Print tokens from the CST";
        "      --pretty     Pretty-print the input";
        "      --recovery   Enable error recovery";
        "      debug options:";
        "      --trace-recovery [output_file]";
        "                   Enable verbose printing of intermediate steps of error recovery algorithm to";
        "                   output_file if it is passed or stdout otherwise"
      ] in
      begin
        Buffer.add_string buffer (String.concat "\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* Specifying the command-line options a la GNU *)

    let mono       = ref false
    and pretty     = ref false
    and cst        = ref false
    and cst_tokens = ref false
    and recovery   = ref false
    (* debug options *)
    and trace_recovery        = ref false
    and trace_recovery_output = ref None

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
        noshort, "mono",       set mono true, None;
        noshort, "pretty",     set pretty true, None;
        noshort, "cst",        set cst true, None;
        noshort, "cst-tokens", set cst_tokens true, None;
        noshort, "recovery",   set recovery true, None;
        noshort, "trace-recovery", set trace_recovery true,
          Some (fun path -> trace_recovery := true;
                            trace_recovery_output := Some path);

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
       status of the previous CLI (here, [Lexer_CLI.status]). *)

    module SSet = Set.Make (String)

    let opt_wo_arg =
      let open SSet in
      empty
      |> add "--mono"
      |> add "--pretty"
      |> add "--cst"
      |> add "--cst-tokens"
      |> add "--recovery"
      |> add "--trace-recovery"

      (* The following options are present in all CLI *)
      |> add "--cli"
      |> add "--help" |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg = SSet.empty
                       |> SSet.add "--trace-recovery"

    let argv_copy = Array.copy Sys.argv

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg

    type status = [
      Lexer_CLI.status
    | `DependsOnOtherOption of string * string
    ]

    let status = (Lexer_CLI.status :> status)

    let status =
      try
        Getopt.parse_cmdline specs anonymous; status
      with Getopt.Error msg -> `SyntaxError msg

    let () =
      for i = 0 to Array.length Sys.argv - 1 do
        Sys.argv.(i) <- argv_copy.(i)
      done

    (* Re-exporting immutable fields with their CLI value *)

    let mono       = !mono
    and pretty     = !pretty
    and cst        = !cst
    and cst_tokens = !cst_tokens
    and recovery   = !recovery
    (* Debug options *)
    and trace_recovery        = !trace_recovery
    and trace_recovery_output = !trace_recovery_output

    (* Re-exporting and printing on stdout the CLI options *)

    let make_cli buffer : Buffer.t =
      (* Options "help", "version" and "cli" are not given. *)
      let options = [
        sprintf "mono       = %b" mono;
        sprintf "pretty     = %b" pretty;
        sprintf "cst        = %b" cst;
        sprintf "cst_tokens = %b" cst_tokens;
        sprintf "recovery   = %b" recovery;
        sprintf "trace_recovery = %b" trace_recovery;
        sprintf "trace_recovery_output = %s" @@
            Option.value trace_recovery_output ~default:"None"] in
    begin
      Buffer.add_string buffer (String.concat "\n" options);
      Buffer.add_char   buffer '\n';
      buffer
    end

    (* Checking combinations of options *)

    let status =
      match mono, pretty, cst, cst_tokens, recovery, trace_recovery with
      |     _,  true,  true,     _,     _,     _ -> `Conflict ("--pretty", "--cst")
      |     _,  true,     _,  true,     _,     _ -> `Conflict ("--pretty", "--cst-tokens")
      |     _,     _,  true,  true,     _,     _ -> `Conflict ("--cst", "--cst-tokens")
      |  true,     _,     _,     _,  true,     _ -> `Conflict ("--mono", "--recovery")
      |     _,     _,     _,     _, false,  true -> `DependsOnOtherOption
                                                      ("--trace-recovery", "--recovery")
      |     _,     _,     _,     _,     _,     _ -> status


    (* Status *)

    let status =
      match status with
        `Help buffer  -> `Help (make_help buffer)
      | `CLI buffer   -> `CLI (make_cli buffer)
      | `Version _    -> `Version Version.version
      | _             -> status
  end
