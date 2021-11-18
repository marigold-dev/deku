(* Parsing the command-line options *)

(* Vendor dependencies *)

module Argv = Simple_utils.Argv

(* The signature [COMMENTS] specifies the kind of comments
   expected. Those fields do not correspond to CLI (command-line
   options), as they are for internal use. *)

module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end

(* Command-Line Interface (CLI) options *)

module type S =
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

(* Parsing the command line options *)

module Make (Comments: COMMENTS) : S =
  struct
    include Comments

    (* Auxiliary functions and modules *)

    let sprintf = Printf.sprintf

    let split_at_colon = Str.(split (regexp ":"))

    let add_path dirs path = dirs := !dirs @ split_at_colon path

    let make_help () : Buffer.t =
      let file   = Filename.basename Sys.argv.(0) in
      let buffer = Buffer.create 203 in
      let header =
        sprintf "Usage: %s [<option> ...] -- [<input>]\n\
                 where <input> is the source file (default: stdin),\n\
                 and each <option> (if any) is one of the following:\n"
                file
      and options = [
        "  -I <paths>       Inclusion paths (colon-separated)";
        "  -h, --help       This help";
        "  -v, --version    Commit hash on stdout";
        "      --cli        Print given options (debug)";
        "      --columns    Columns for source locations";
        "      --show-pp    Print result of preprocessing"
      ] in
      begin
        Buffer.add_string buffer header;
        Buffer.add_string buffer (String.concat "\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* Specifying the command-line options a la GNU *)

    let input    = ref None
    and dirs     = ref []
    and columns  = ref false
    and show_pp  = ref false

    and help     = ref false
    and version  = ref false
    and cli      = ref false

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
      let open Getopt in [
        'I',     nolong,    None, Some (add_path dirs);
        noshort, "columns", set columns true, None;
        noshort, "show-pp", set show_pp true, None;

        noshort, "cli",     set cli true, None;
        'h',     "help",    set help true, None;
        'v',     "version", set version true, None;
      ]

    (* Handler of anonymous arguments.

       The test on [arg] is for the hack below not to trigger the
       error "Multiple inputs" for erased options. *)

    let anonymous arg =
      if arg <> "" then
        match !input with
          None -> input := Some arg
        | Some _ -> raise (Getopt.Error "Multiple inputs.")

    (* Parsing the command-line options *)

    (* We do not want the exception [Getopt.Error] to be raised when
       finding an unknown option.

         The following is a hack to filter out unknown options but
       leaving correct ones, even if their syntax is invalid and will
       result in an exception [Getopt.Error] raised by
       [Getopt.parse_cmdline] below.

         IMPORTANT: We assume that there are no concatenated short
       options (here, the only possible combinations are "-hv" and
       "-vh") and that anonymous arguments (here, a unique text file)
       is given after "--".

         First, we make a backup copy of [Sys.argv]. Second, we filter
       it in a list by calling [Argv.filter]. That function performs a
       side effect on [Sys.argv]: unknown options are removed and
       compacted. That is why [Sys.argv] has to be restored from the
       backup after parsing the command-line: another parse is now
       possible by another client. *)

    module SSet = Set.Make (String)

    let opt_wo_arg =
      let open SSet in
      empty
      |> add "--show-pp"
      |> add "--columns"

      (* The following options are present in all CLI *)
      |> add "--cli"                  (* For debugging *)
      |> add "--help" |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg =
      let open SSet in
      empty
      |> add "-I"

    let argv_copy = Array.copy Sys.argv

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg

    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    let status =
      try
        Getopt.parse_cmdline specs anonymous;
        `Done (* Default. Other values assigned below. *)
      with Getopt.Error msg -> `SyntaxError msg

    let () =
      for i = 0 to Array.length Sys.argv - 1 do
        Sys.argv.(i) <- argv_copy.(i)
      done

    (* Re-exporting immutable fields with their CLI value *)

    let dirs    = !dirs
    and offsets = not !columns
    and show_pp = !show_pp
    and help    = !help
    and version = !version

    let string_of_opt convert = function
      None -> "None"
    | Some x -> sprintf "Some %S" (convert x)

    let string_of_dirs = sprintf "[%s]" (String.concat ";" dirs)

    (* Re-exporting and printing on stdout the CLI options *)

    let cli_buffer = Buffer.create 131

    let () =
      (* Options "help", "version" and "cli" are not given. *)
      let options = [
        "CLI options";
        sprintf "input      = %s" (string_of_opt (fun x -> x) !input);
        sprintf "dirs       = %s" string_of_dirs;
        sprintf "show-pp    = %b" show_pp;
        sprintf "columns    = %b" (not offsets)
      ] in
    begin
      Buffer.add_string cli_buffer (String.concat "\n" options);
      Buffer.add_char   cli_buffer '\n'
    end

    (* Input and status *)

    let input, status =
      match status with
        `SyntaxError _  -> !input, status
      | _ when help     -> !input, `Help (make_help ())
      | _ when !cli     -> !input, `CLI cli_buffer
      | _ when version  -> !input, `Version Version.version
      | _ -> match !input with
               None | Some "-" -> None, `Done
             | Some file_path ->
                 !input,
                 if   Sys.file_exists file_path
                 then `Done
                 else `FileNotFound "Source file not found."

    (* File extension (must come after handling of input above) *)

    let extension =
      match input with
        None -> None
      | Some file_path ->
          let x = Filename.extension file_path
          in if x = "" then None else Some x
   end
