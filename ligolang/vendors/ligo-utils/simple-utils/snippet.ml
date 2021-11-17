(* used to show code snippets in error messages *)

let print_code ppf (l:Region.t) (input_line: unit -> string) =
  let dumb = Unix.getenv "TERM" = "dumb" in
  let start = l#start#line in
  let start_column = l#start#offset `Byte in
  let stop = l#stop#line in
  let stop_column = l#stop#offset `Byte in
  let rec loop_lines curr start stop =
    try (
      let curr = curr + 1 in
      let line = input_line () in
      let line_length = String.length line in
      (if curr >= start - 1 && curr < stop + 2 then (
        let _ = Format.fprintf ppf "%3i" curr in
        if curr >= start && curr <= stop then (
          if curr > start && curr < stop then
            Format.fprintf ppf  (
              if dumb then
                " | %s%!"
              else
                " | \027[1m\027[31m%s\027[0m%!")
              (line  ^ "\n")
          else if curr = start then (
            let before = String.sub line 0 start_column in
            Format.fprintf ppf " | %s" before;
            if curr = stop then (
              let between = String.sub line start_column (stop_column - start_column) in
              let after = String.sub line stop_column (line_length - stop_column) in
              Format.fprintf ppf (
                if dumb then
                  "%s%!%s\n"
                else
                  "\027[1m\027[31m%s\027[0m%!%s\n")
                between
                after
            ) else (
              let after = String.sub line start_column (line_length - start_column) in
              if dumb then
                Format.fprintf ppf "%s%!\n" after
              else
                Format.fprintf ppf "\027[1m\027[31m%s\027[0m%!\n" after
            )
          )
          else if curr = stop then (
            let before = String.sub line 0 stop_column in
            let after = String.sub line stop_column (line_length - stop_column) in
            Format.fprintf ppf " | ";
            if dumb then
              Format.fprintf ppf "%s%!%s\n" before after
            else
              Format.fprintf ppf "\027[1m\027[31m%s\027[0m%!%s\n" before after
          )
        )
        else
          Format.fprintf ppf "%s" (" | " ^ line ^ "\n"));
        );
        if curr < stop + 2 then
          loop_lines curr start stop
        ) with
      | _ -> ()
    in
    loop_lines 0 start stop

let regexp = Str.regexp "\n"

let pp ppf (loc: Location.t) =
  match loc with
  | File l -> (
    if l#file <> "" then
      Format.fprintf ppf "%s:\n" (l#to_string `Byte);
    try
      let in_ = open_in l#file in
      let result = print_code ppf l (fun () -> input_line in_) in
      close_in in_;
      result
    with | _ ->
      ())
  | _ ->
    Location.pp ppf loc

let lift : Region.region -> Location.t = fun x -> File x
let pp_lift = fun ppf r -> pp ppf @@ lift r
