type face_option = 
  Bold
| Background of string
| Foreground of string
| Inherit of string

type terminal = 
  Background_dark of face_option list
| Background_light of face_option list
| All of face_option list

type face = {
  name: string;
  terminals: terminal list;
  description: string;
}

module LigoFontLock = struct
  let character = {
    name = "ligo-font-lock-character-face";
    terminals = [
      All [Inherit "font-lock-string-face"]
    ];
    description = "Face description for characters."
  }
  let number = {
    name = "ligo-font-lock-number-face";
    terminals = [
      All [
        Inherit "default"
      ]
    ];
    description = "Face description for numbers."
  }
  let float_ = {
    name = "ligo-font-lock-float-face";
    terminals = [
      All [Inherit "default"]
    ];
    description = "Face description for floats."
  }
  let builtin_function = {
    name = "ligo-font-lock-builtin-function-face";
    terminals = [
      All [Inherit "font-lock-function-name-face"]
    ];
    description = "Face description for builtin functions."
  }
  let statement = {
    name = "ligo-font-lock-statement-face";
    terminals = [
      All [Inherit "font-lock-keyword-face"]
    ];
    description = "Face description for statements."
  }
  let conditional = {
    name = "ligo-font-lock-conditional-face";
    terminals = [
      All [Inherit "font-lock-keyword-face"]
    ];
    description = "Face description for conditionals."
  }
  let repeat = {
    name = "ligo-font-lock-repeat-face";
    terminals = [
      All [Inherit "font-lock-keyword-face"]
    ];
    description = "Face description for repeat keywords."
  }
  let label = {
    name = "ligo-font-lock-label-face";
    terminals = [
      Background_dark [
        Foreground "#eedd82"
      ];
      All [
        Inherit "font-lock-function-name-face"
      ]
    ];
    description = "Face description for labels."
  }
  let operator = {
    name = "ligo-font-lock-operator-face";
    terminals = [
      All [
        Inherit "default"
      ]
    ];
    description = "Face description for operators."
  }
  let exception_ = {
    name = "ligo-font-lock-exception-face";
    terminals = [
      Background_light [
        Foreground "dark orange"
      ];
      Background_dark [
        Foreground "orange"
      ]
    ];
    description = "Face description for exceptions."
  }
  let builtin_type = {
    name = "ligo-font-lock-builtin-type-face";
    terminals = [
      All [Inherit "font-lock-type-face"]
    ];
    description = "Face description for builtin types."
  }
  let storage_class = {
    name = "ligo-font-lock-storage-class-face";
    terminals = [
      All [
        Inherit "font-lock-keyword-face";
      ]
    ];
    description = "Face description for storage classes."
  }
  let builtin_module = {
    name = "ligo-font-lock-builtin-module-face";
    terminals = [
      All [
        Inherit "font-lock-function-name-face"
      ]
    ];
    description = "Face description for builtin modules."
  }
  let structure = {
    name = "ligo-font-lock-structure-face";
    terminals = [All [Inherit "font-lock-constant-face"]];
    description = "Face description for structures."
  }
  let type_def = {
    name = "ligo-font-lock-type-def-face";
    terminals = [
      All [Inherit "font-lock-type-face"]
    ];
    description = "Face description for type definitions."
  }
  let special_char = {
    name = "ligo-font-lock-special-char-face";
    terminals = [
      All [Inherit "font-lock-string-face"]
    ];
    description = "Face description for special characters."
  }
  let special_comment = {
    name = "ligo-font-lock-special-comment-face";
    terminals = [
      All [Inherit "font-lock-comment-face"]
    ];
    description = "Face description for special comments."
  }
  let error = {
    name = "ligo-font-lock-error-face";
    terminals = [
      All [Inherit "error"]
    ];
    description = "Face description for errors."
  }
  let todo = {
    name = "ligo-font-lock-todo-face";
    terminals = [
      All [Inherit "highlight"]
    ];
    description = "Face description for todos."
  }
  let attribute = {
    name = "ligo-font-lock-attribute-face";
    terminals = [
      All [Inherit "font-lock-preprocessor-face"]
    ];
    description = "Face description for todos."
  }
end

let highlight_to_opt = function
    Core.Comment -> Some "font-lock-comment-face"
  | Constant         -> Some "font-lock-constant-face"
  | String           -> Some "font-lock-string-face"
  | Character        -> Some LigoFontLock.character.name
  | Number           -> Some LigoFontLock.number.name
  | Boolean          -> Some "font-lock-constant-face"
  | Float            -> Some LigoFontLock.float_.name
  | FunctionName     -> Some "font-lock-variable-name-face"
  | Identifier       -> Some "font-lock-variable-name-face" 
  | Builtin_function -> Some LigoFontLock.builtin_function.name
  | Function         -> Some "font-lock-function-name-face"
  | Statement        -> Some LigoFontLock.statement.name
  | Conditional      -> Some LigoFontLock.conditional.name
  | Repeat           -> Some LigoFontLock.repeat.name
  | Label            -> Some LigoFontLock.label.name
  | Operator         -> Some LigoFontLock.operator.name
  | Keyword          -> Some "font-lock-keyword-face"
  | Exception        -> Some LigoFontLock.exception_.name
  | PreProc          -> Some "font-lock-preprocessor-face"
  | Builtin_type     -> Some LigoFontLock.builtin_type.name
  | Type             -> Some "font-lock-type-face"
  | StorageClass     -> Some LigoFontLock.storage_class.name
  | Builtin_module   -> Some LigoFontLock.builtin_module.name
  | Structure        -> Some LigoFontLock.structure.name
  | Typedef          -> Some LigoFontLock.type_def.name
  | SpecialChar      -> Some LigoFontLock.special_char.name
  | SpecialComment   -> Some LigoFontLock.special_comment.name
  | Underlined       -> Some "underline"
  | Error            -> Some LigoFontLock.error.name
  | Todo             -> Some LigoFontLock.todo.name
  | Attribute        -> Some LigoFontLock.attribute.name

let highlight_to_opt = function 
  Some s -> highlight_to_opt s
| None -> None

module Print = struct
  open Format 

  let print_face_option: formatter -> face_option -> unit = fun fmt -> function
    Bold -> fprintf fmt ":bold "
  | Background c ->
    fprintf fmt ":background \"%s\"" c;
    fprintf fmt " "
  | Foreground c ->
    fprintf fmt ":foreground \"%s\"" c;
    fprintf fmt " "
  | Inherit s ->
    fprintf fmt ":inherit %s " s

  let print_terminal fmt = function
    All f -> 
      fprintf fmt "\t\t(t (";
      List.iter (print_face_option fmt) f;
      fprintf fmt "))\n"
  | Background_dark f ->
      fprintf fmt "\t\t(((background dark)) (";
      List.iter (print_face_option fmt) f;
      fprintf fmt "))\n"
  | Background_light f ->
      fprintf fmt "\t\t(((background light)) (";
      List.iter (print_face_option fmt) f;
      fprintf fmt "))\n"

  let print_face: formatter -> face -> unit = fun fmt face ->
    fprintf fmt "(defface %s\n" face.name;
    fprintf fmt "\t'(\n";
    List.iter (print_terminal fmt) face.terminals;
    fprintf fmt "\t)\n";
    fprintf fmt "\t\"%s\"\n" face.description;
    fprintf fmt "\t:group 'ligo\n";
    fprintf fmt ")\n";
    fprintf fmt "(defvar %s\n" face.name;
    fprintf fmt "\t'%s)\n\n" face.name
    
  let print_faces fmt =
    let faces = [
      LigoFontLock.attribute;
      LigoFontLock.character;
      LigoFontLock.number;
      LigoFontLock.float_;
      LigoFontLock.builtin_function;
      LigoFontLock.statement;
      LigoFontLock.conditional;
      LigoFontLock.repeat;
      LigoFontLock.label;
      LigoFontLock.operator;
      LigoFontLock.exception_;
      LigoFontLock.builtin_type;
      LigoFontLock.storage_class;
      LigoFontLock.builtin_module;
      LigoFontLock.structure;
      LigoFontLock.type_def;
      LigoFontLock.special_char;
      LigoFontLock.special_comment;
      LigoFontLock.error;
      LigoFontLock.todo
    ]
    in
    List.iter (fun f -> print_face fmt f) faces

  let print_syntax_table fmt syntax syntax_table = 
    fprintf fmt "(defun %s-syntax-table ()\n" syntax;
    fprintf fmt "\t\"Syntax table\"\n";
    fprintf fmt "\t(let ((st (make-syntax-table)))\n";
    List.iter (fun (c, s) -> 
      fprintf fmt "\t(modify-syntax-entry ?%s \"%s\" st)\n" c s
    ) syntax_table;
    fprintf fmt "\tst))\n\n"

  let highlight fmt index match_name =
    match highlight_to_opt match_name with 
    | Some highlight ->
        fprintf fmt "\t\t\t(%s %s)\n" index highlight
    | None -> 
      ()
    
  let print_font_lock fmt syntax repository =
    fprintf fmt "(defvar %s-font-lock-defaults\n" syntax;
    fprintf fmt "\t`(\n";
    List.iter (fun i -> 
      match i.Core.kind with 
        Match {match_; match_name} ->
          let regexps, highlights = List.split match_ in
          let match_regexp = String.concat "" (List.map (fun f -> f.Core.emacs) regexps) in
          fprintf fmt "\t\t(,\"%s\"\n" match_regexp;
          (match highlight_to_opt match_name with 
          | Some highlight ->
              fprintf fmt "\t\t\t. %s\n" highlight
          | None -> 
            ());
          let rec aux index = function 
            s :: rest -> 
              (match highlight_to_opt s with 
                  Some highlight ->
                    fprintf fmt "\t\t\t(%i %s)\n" index highlight
                | None -> 
                  ()
              );
              aux (index + 1) rest
          | [] -> ()
          in 
          aux 1 highlights;
          fprintf fmt "\t\t)\n"

      | Begin_end {begin_; end_; meta_name; _} ->
        ignore end_;
        let highlight_opt_to_string no opt =
          match highlight_to_opt opt with 
            Some highlight -> no ^ highlight
          | None -> ""
        in
        let all = highlight_opt_to_string " . " meta_name in
        let rec aux regexp_begin highlights counter = function
          (regexp, highlight) :: rest ->
            let regexp = regexp_begin ^ regexp.Core.emacs in
            let highlights = highlights ^ highlight_opt_to_string (" " ^ string_of_int counter ^ " ") highlight in
            aux regexp highlights (counter + 1) rest
        | [] -> 
          (regexp_begin, "(" ^ highlights ^ ")")
        in
        let regexp_begin, highlights = aux "" "" 1 begin_ in
        let highlights = if highlights = "" then all else highlights  in
        fprintf fmt "\t\t(,\"%s\" %s)\n" (regexp_begin) highlights
    ) repository;
    fprintf fmt "\t)\n";
    fprintf fmt "\t\"Syntax highlighting rules for %s\")\n" syntax
(* 

For debugging.
  let print_custom_faces fmt = 
    fprintf fmt "(custom-set-faces\n";
    fprintf fmt "\t'(font-lock-function-name-face ((t (:foreground \"#00dd44\"))))\n";
    fprintf fmt "\t'(ligo-font-lock-storage-class-face ((t (:foreground \"#FF00FF\"))))\n";
    fprintf fmt "\t'(font-lock-preprocessor-face ((t (:foreground \"#CCFF22\"))))\n";
    fprintf fmt "\t'(ligo-font-lock-label-face ((t (:foreground \"#1155FF\"))))\n";
    fprintf fmt "\t'(ligo-font-lock-number-face ((t (:foreground \"#FF3311\"))))\n";
    
    fprintf fmt ")\n" *)
  

  let print fmt syntax alt_name (t: Core.t) =
    (* print_custom_faces fmt; *)
    print_faces fmt;
    let Core.{operators; string_delimiters; syntax_table; _} = t.language_features in
    let syntax_table = [
      ("_", "w");
      ("'", "_");
      (".", "'");
    ]
    @ 
    (List.fold_left (fun a o -> if String.length o = 1 then (o, ".") :: a else a) [] operators)
    @
    (List.map (fun l -> (l.Core.emacs, "\\\"")) string_delimiters)
    @
    syntax_table
    in
    print_syntax_table fmt syntax syntax_table;
    print_font_lock fmt syntax t.repository;

    fprintf fmt "(defun %s-reload ()\n" syntax;
    fprintf fmt "\t\"Reload the %s-mode code and re-apply the default major mode in the current buffer.\"\n" syntax;
    fprintf fmt "\t(interactive)\n";
    fprintf fmt "\t(unload-feature '%s-mode)\n" syntax; 
    fprintf fmt "\t(require '%s-mode)\n" syntax;
    fprintf fmt "\t(normal-mode))\n\n";

    fprintf fmt "(define-derived-mode ligo-%s-mode prog-mode \"%s\"\n" alt_name syntax;
    fprintf fmt "\t\"Major mode for writing %s code.\"\n" syntax;
    fprintf fmt "\t(setq font-lock-defaults '(%s-font-lock-defaults))\n" syntax;
    fprintf fmt "\t(set-syntax-table (%s-syntax-table)))\n\n" syntax;

    
    fprintf fmt "(add-to-list 'auto-mode-alist '(\"\\\\.%s\\\\'\" . ligo-%s-mode))\n" syntax alt_name;
    fprintf fmt "(provide '%s-mode)\n" syntax;
end


let to_emacs t =
  let buffer = Buffer.create 100 in
  let open Format in
  let fmt = formatter_of_buffer buffer in
  let name = match Filename.extension t.Core.scope_name with 
      "" -> t.scope_name
    | a -> String.sub a 1 (String.length a - 1)
  in
  Print.print fmt name t.alt_name t;
  Buffer.contents buffer