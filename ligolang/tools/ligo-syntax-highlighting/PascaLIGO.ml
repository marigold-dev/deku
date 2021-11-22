module Core     = SyntaxHighlighting.Core
module Helpers  = SyntaxHighlighting.Helpers

module Name = struct
  let attribute              = "attribute"
  let macro                  = "macro"
  let control_keywords       = "controlkeywords"
  let function_              = "function"
  let operators              = "operators"
  let type_definition        = "typedefinition"
  let module_                = "module"
  let identifier_constructor = "identifierconstructor"
  let const_or_var           = "constorvar"
  let numeric_literals       = "numericliterals"
end

let syntax_highlighting = 
  let open Core in
  {
    syntax_name          = "ligo";
    alt_name             = "pascal";
    scope_name           = "source.ligo";
    file_types           = [];
    folding_start_marker = None;
    folding_stop_marker = None;
    language_features = {
      operators = [
        "not";
        "mod";
        "ediv";
        "+";
        "-";
        "*";
        "/";
        "==";
        "=/=";
        ">=";
        "<=";
        ">";
        "<";
        "#";
        "or";
        "and";
        "^"
      ];
      string_delimiters = [
        {
          emacs    = "\\\"";
          textmate = "\\\"";
          vim      = "\\\""
        }
      ];
      comments = {
        line_comment = {
          emacs    = "//";
          textmate = "\\/\\/.*$";
          vim      = "\\/\\/.*$"
        };
        block_comment = (
          {
            emacs    = "(*";
            textmate = "\\(\\*";
            vim      = "(\\*"
          },
          {
            emacs    = "*)";
            textmate = "\\*\\)";
            vim      = "\\*)"
          }
        );
      };
      brackets = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
      ];
      auto_closing_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
        ("(*", "*)");
        ("begin", "end");
      ];
      surrounding_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
        ("(*", "*)");
        ("begin", "end");
      ];
      syntax_table = [
        ("\\n", "> b");
        ("/", ". 12b");
        ("*", ". 23");
        ("\\(", "()1n");
        ("\\)", ")(4n");
      ]
    };
    syntax_patterns = [
      Name.attribute;
      Name.macro;
      Name.control_keywords;
      Name.function_;
      Name.operators;
      Name.type_definition;
      Name.module_;
      Name.identifier_constructor;
      Name.const_or_var;
      Name.numeric_literals;
    ];
    repository = [
      Helpers.attribute;
      Helpers.macro;
      {
        name = Name.control_keywords;
        kind = Match {
          match_name = Some Conditional;
          match_ = [(Regexp.control_keywords_match_ligo, None)]
        }
      };
      {
        name = Name.function_;
        kind = Match {
          match_ = [
            (Regexp.let_binding_match1_ligo, Some Keyword);
            (Regexp.let_binding_match2_ligo, Some FunctionName)
          ];
          match_name = None
        }
      };
      Helpers.numeric_literals;
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.operators_match_ligo, None)]
        }
      };
      {
        name = Name.type_definition;
        kind = Match {
          match_ = [(Regexp.type_definition_match, None)];
          match_name = Some Type
        }
      };
      {
        name = Name.module_;
        kind = Match {
          match_     = [
            (Regexp.module_match1, Some Structure);
            (Regexp.module_match2, Some Identifier)
          ];
          match_name = None
        }
      };
      {
        name = Name.identifier_constructor;
        kind = Match {
          match_name = None;
          match_     = [(Regexp.identifier_constructor_match, Some Label)]
        }
      };
      {
        name = Name.const_or_var;
        kind = Match { 
          match_name = None;
          match_     = [(Regexp.const_or_var, Some Keyword)]
        }
      }
    ]
  } 
