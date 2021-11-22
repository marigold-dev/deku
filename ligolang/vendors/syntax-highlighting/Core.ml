type t = {
  syntax_name:                string;
  alt_name:                   string;
  file_types:                 string list;
  scope_name:                 string;
  folding_start_marker:       string option; (* todo string -> regexp *)
  folding_stop_marker:        string option; (* todo string -> regexp *)
  language_features:          language_features;
  syntax_patterns:            string list;
  repository:                 pattern list
}

and regexp = {
  emacs:    string;
  textmate: string;
  vim:      string;
}

and language_features = {
  operators: string list;
  string_delimiters: regexp list;
  comments: language_features_comments;
  brackets: (string * string) list;
  auto_closing_pairs: (string * string) list;
  surrounding_pairs: (string * string) list;
  syntax_table: (string * string) list; (* for Emacs *)
}

and language_features_comments = {
  line_comment:  regexp;
  block_comment: (regexp * regexp)
}

and highlight_name = 
  Comment
| Attribute
| Constant
| String
| Character
| Number
| Boolean
| Float
| Identifier
| Function
| Statement
| Conditional
| Repeat
| Label
| Operator
| Keyword
| Exception
| PreProc
| Type
| StorageClass
| Structure
| Typedef
| SpecialChar
| SpecialComment
| Underlined
| Error
| Todo

| Builtin_type
| Builtin_module
| Builtin_function
| FunctionName

and error = 
  Referenced_rule_does_not_exist of string
| Meta_name_some_but_empty of string
| Begin_cant_be_empty of string
| End_cant_be_empty of string

and pattern = {
  name: string;
  kind: pattern_kind;
}

and pattern_kind = 
  Begin_end of begin_end_pattern
| Match     of match_pattern

and begin_end_pattern = {
  meta_name:      highlight_name option;
  begin_:         (regexp * highlight_name option) list;
  end_:           (regexp * highlight_name option) list;
  patterns:       string list;
}

and match_pattern = {
  match_:   (regexp * highlight_name option) list;
  match_name: highlight_name option
}