
let macro: Core.pattern = {
  name = "macro";
  kind = Match {
    match_ = [(Regexp.macro_match, None)];
    match_name = Some PreProc
  }
}

let string: Core.pattern list = [
  {
    name = "string_specialchar";
    kind = Match {
      match_ = [(Regexp.string_special_char_match, None)];
      match_name = Some SpecialChar
    }
  };
  {
    name = "string";
    kind = Begin_end {
      meta_name = Some String;
      begin_ = [(Regexp.string_begin, None)];
      end_ = [(Regexp.string_end, None)];
      patterns = [
        "string_specialchar"
      ]
    }
  }
]
  let ocaml_comment: Core.pattern list = [
    {
      name = "line_comment";
      kind = Match {
        match_name = Some Comment;
        match_ = [(Regexp.ocaml_line_comment_match, None)]
      };
    };
    { name = "block_comment";
      kind = Begin_end {
        meta_name = Some Comment;
        begin_ = [(Regexp.ocaml_block_comment_begin, None)];
        end_ = [(Regexp.ocaml_block_comment_end, None)];
        patterns = []
      }
    }
  ]

  let c_comment: Core.pattern list = [
    { name = "line_comment";
      kind = Match {
        match_name = Some Comment;
        match_ = [(Regexp.c_line_comment_match, None)];
      }
    };
    { name = "block_comment";
      kind = Begin_end {
        meta_name = Some Comment;
        begin_ = [(Regexp.c_block_comment_begin, None)];
        end_ = [(Regexp.c_block_comment_end, None)];
        patterns = []
      }
    }
  ]

  let numeric_literals: Core.pattern = {
    name = "numericliterals";
    kind = Match {
      match_name = Some Number;
      match_ = [(Regexp.numeric_literals_match, None)]
    }
  }

let attribute: Core.pattern = {
  name = "attribute";
  kind = Match {
    match_name = Some Attribute;
    match_ = [(Regexp.attributes_match, None)]
  }
}