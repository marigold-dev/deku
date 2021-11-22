let macro_match: Core.regexp = {
  emacs    = "^\\\\(#[a-zA-Z]+\\\\)";
  textmate = "^\\#[a-zA-Z]+";
  vim      = "^\\#[a-zA-Z]\\+"
}

let string_special_char_match: Core.regexp = {
  emacs    = "\\\\.";
  textmate = "";
  vim      = ""
}

let string_begin: Core.regexp = {
  emacs    = "\\\\\\\"";
  textmate = "";
  vim      = ""
}

let string_end: Core.regexp = {
  emacs    = "\\\\\\\"";
  textmate = "";
  vim      = ""
}

let ocaml_line_comment_match: Core.regexp = {
  emacs    = "";
  textmate = "";
  vim      = ""
}

let ocaml_block_comment_begin: Core.regexp = {
  emacs    = "";
  textmate = "";
  vim      = ""
}

let ocaml_block_comment_end: Core.regexp = {
  emacs    = "";
  textmate = "";
  vim      = ""
}

let c_line_comment_match: Core.regexp = {
  emacs    = "";
  textmate = "";
  vim      = ""
}

let c_block_comment_begin: Core.regexp = {
  emacs    = "";
  textmate = "";
  vim      = ""
}

let c_block_comment_end: Core.regexp = {
  emacs    = "";
  textmate = "";
  vim      = ""
}

let numeric_literals_match: Core.regexp = {
  emacs    = "\\\\b[-+]?\\\\([0-9]+\\\\)\\\\(n\\\\|\\\\tz\\\\|tez\\\\|mutez\\\\|\\\\)\\\\b";
  textmate = "(\\+|\\-)?[0-9]+(n|tz|tez|mutez|)\\b";
  vim      = "\\<[0-9]+\\(n\\|tz\\|tez\\|mutez\\|\\)\\>"
}

let attributes_match: Core.regexp = {
  emacs    = "\\\\[@.*\\\\]";
  textmate = "\\[@.*\\]";
  vim      = "\\[@.*\\]"
}