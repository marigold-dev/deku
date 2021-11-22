module Core = SyntaxHighlighting.Core

let let_binding_match1: Core.regexp = {
  emacs    = "\\\\b\\\\(let\\\\)\\\\b[ ]*"; 
  textmate = "\\b(let)\\b\\s*";
  vim      = "\\(let\\)\\W";
}

let let_binding_match2: Core.regexp = {
  emacs    = "\\\\b\\\\(rec\\\\|\\\\)\\\\b[ ]*"; 
  textmate = "\\b(rec|)\\s*";
  vim      = "rec\\W\\|";
}

let let_binding_match3: Core.regexp = {
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\|\\\\)"; 
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)";
  vim      = "[a-zA-Z$_][a-zA-Z0-9$_]*";
}
let let_binding_match1_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\(function\\\\)\\\\b[ ]*"; 
  textmate = "\\b(function)\\b\\s*";
  vim      = "\\(function\\)\\W";
}

let let_binding_match2_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\|\\\\)"; 
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)";
  vim      = "[a-zA-Z$_][a-zA-Z0-9$_]*";
}

let lambda_begin: Core.regexp = {
  emacs    = "\\\\b\\\\(fun\\\\)\\\\b";
  textmate = "\\b(fun)\\b";
  vim      = "\\(fun\\)\\W"
}

let lambda_end: Core.regexp = {
  emacs    = "(->)";
  textmate = "(->)";
  vim      = "\\(->\\)"
}

let control_keywords_match: Core.regexp = {
  emacs    = "\\\\b\\\\(match\\\\|with\\\\|if\\\\|then\\\\|else\\\\|assert\\\\|failwith\\\\|begin\\\\|end\\\\|in\\\\)\\\\b";
  textmate = "\\b(match|with|if|then|else|assert|failwith|begin|end|in)\\b";
  vim      = "\\<\\(match\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|end\\|in\\)\\>"
}

let control_keywords_match_reasonligo: Core.regexp = {
  emacs    = "\\\\b\\\\(switch\\\\|if\\\\|else\\\\|assert\\\\|failwith\\\\)\\\\b";
  textmate = "\\b(switch|if|else|assert|failwith)\\b";
  vim      = "\\<\\(switch\\|if\\|else\\|assert\\|failwith\\)\\>"
}

let control_keywords_match_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\(case\\\\|with\\\\|if\\\\|then\\\\|else\\\\|assert\\\\|failwith\\\\|begin\\\\|end\\\\|in\\\\|is\\\\|from\\\\|skip\\\\|block\\\\|contains\\\\|to\\\\|step\\\\|of\\\\|while\\\\|for\\\\|remove\\\\)\\\\b";
  textmate = "\\b(case|with|if|then|else|assert|failwith|begin|end|in|is|from|skip|block|contains|to|step|of|while|for|remove)\\b";
  vim      = "\\<\\(case\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|end\\|in\\|is\\|from\\|skip\\|block\\|contains\\|to\\|step\\|of\\|while\\|for\\|remove\\)\\>"
}

let operators_match: Core.regexp = {
  emacs    = "[ ]*\\\\(::\\\\|-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|<>\\\\|<=\\\\|>=\\\\)[ ]*";
  textmate = "\\s+(::|\\-|\\+|mod|land|lor|lxor|lsl|lsr|&&|\\|\\||>|<>|<=|=>|<|>)\\s+";
  vim      = "\\<\\(::\\|-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|<>\\|<=\\|>=\\)\\>"
}

let operators_match_reasonligo: Core.regexp = {
  emacs    = "[ ]*\\\\(-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|!=\\\\|<=\\\\|>=\\\\)[ ]*";
  textmate = "\\s+(\\-|\\+|mod|land|lor|lxor|lsl|lsr|&&|\\|\\||>|!=|<=|=>|<|>)\\s+";
  vim      = "\\<\\(-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|!=\\|<=\\|>=\\)\\>"
}

let operators_match_ligo: Core.regexp = {
  emacs    = "[ ]*\\\\(-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|=/=\\\\|<=\\\\|>=\\\\)[ ]*";
  textmate = "\\s+(\\-|\\+|mod|land|lor|lxor|lsl|lsr|&&|\\|\\||>|=/=|<=|=>|<|>)\\s+";
  vim      = "\\<\\(-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|=/=\\|<=\\|>=\\)\\>"
}


let module_match1: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\.";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\.";
  vim      = "\\<\\([A-Z][a-zA-Z0-9_$]*\\)\\."
}

let module_match2: Core.regexp = {
  emacs    = "\\\\([a-z_][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "([a-z][a-zA-Z0-9_$]*)";
  vim      = "[a-z_][a-zA-Z0-9_$]*"
}


let identifier_constructor_match: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\s+";
  vim      = "\\<\\([A-Z][a-zA-Z0-9_$]*\\)\\s\\+"
}

let type_definition_match: Core.regexp = {
  emacs    = "\\\\b\\\\(type\\\\)\\\\b";
  textmate = "\\b(type)\\b";
  vim      = "\\(type\\)\\>"
}

let type_annotation_match: Core.regexp = {
  emacs    = "\\\\(:[ ]*[^]=;\\\\):]*\\\\)";
  textmate = "(:[ ]*[^\\]=;\\):]*)";
  vim      = "\\(:[^]=;\\):]*\\)"
}

let type_annotation_match_reasonligo: Core.regexp = {
  emacs    = "\\\\(:[ ]*[^,=\n]*\\\\)";
  textmate = "(:[ ]*[^\\]=;\\):]*)";
  vim      = "\\(:[^]=;\\):]*\\)"
}

let multiplication_match: Core.regexp = {
  emacs    = "\\\\(*\\\\)";
  textmate = "(*)";
  vim      = "\\(*\\)"
}

let const_or_var: Core.regexp = {
  emacs    = "\\\\b\\\\(const\\\\|var\\\\)\\\\b";
  textmate = "\\b(const|var)\\b";
  vim      = "\\<\\(const\\|var\\)\\>"

}