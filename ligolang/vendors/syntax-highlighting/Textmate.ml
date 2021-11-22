open Result

let (let*) = Result.bind

module JSON = struct 
 
  let highlight_to_textmate:string -> Core.highlight_name -> string = fun syntax -> function
    Comment           -> "comment.block." ^ syntax
  | Attribute         -> "keyword.control.attribute." ^ syntax
  | Constant          -> "constant.language." ^ syntax
  | String            -> "string.quoted.double." ^ syntax
  | Character         -> "constant.character." ^ syntax
  | Number            -> "constant.numeric." ^ syntax
  | Boolean           -> "constant.language." ^ syntax
  | Float             -> "constant.numeric." ^ syntax
  | Identifier        -> "storage.var." ^ syntax
  | Function          -> "keyword.other.let-binding." ^ syntax
  | Statement         -> "keyword.other." ^ syntax
  | Conditional       -> "keyword.control." ^ syntax
  | Repeat            -> "keyword.control." ^ syntax
  | Label             -> "variable.other.enummember." ^ syntax
  | Operator          -> "keyword.operator." ^ syntax
  | Keyword           -> "keyword.other." ^ syntax
  | Exception         -> "keyword.control." ^ syntax
  | PreProc           -> "meta.preprocessor." ^ syntax
  | Type              -> "entity.name.type." ^ syntax
  | StorageClass      -> "storage.modifier." ^ syntax
  | Structure         -> "storage.class." ^ syntax
  | Typedef           -> "storage.type." ^ syntax
  | SpecialChar       -> "constant.character." ^ syntax
  | SpecialComment    -> "comment.other." ^ syntax
  | Underlined        -> "markup.underline." ^ syntax
  | Error             -> "invalid.illegal." ^ syntax
  | Todo              -> "meta.todo." ^ syntax

  | Builtin_type      -> "support.type." ^ syntax
  | Builtin_module    -> "support.class." ^ syntax
  | Builtin_function  -> "support.function." ^ syntax 
  | FunctionName      -> "entity.name.function." ^ syntax


  
  let rec capture syntax (i: int * Core.highlight_name) = 
    (string_of_int (fst i), `Assoc [("name", `String (highlight_to_textmate syntax (snd i)))])

  and captures syntax (l: (int * Core.highlight_name) list) = 
    `Assoc (List.map (capture syntax) l)

  and pattern_kind (syntax: string) = function 
    Core.Begin_end { 
      meta_name;
      begin_; 
      end_; 
      patterns
    } -> 
      let (_, begin_captures) = 
        List.fold_left 
          (fun (n, result) c -> 
            let _, highlight_name = c in
            match highlight_name with 
              None -> (n + 1, result)
            | Some s -> (n + 1, (n, s) :: result)
          ) 
          (1, []) 
          begin_ 
      in
      let (_, end_captures) = 
        List.fold_left 
          (fun (n, result) c -> 
            let _, highlight_name = c in
            match highlight_name with 
              None -> (n + 1, result)
            | Some s -> (n + 1, (n, s) :: result)
          ) 
          (1, []) 
          end_ 
      in
      let begin_ = String.concat "" (List.map (fun f -> (fst f).Core.textmate) begin_) in
      let end_ = String.concat "" (List.map (fun f -> (fst f).Core.textmate) end_) in
      (match meta_name with 
        Some s -> [("name", `String (highlight_to_textmate syntax s))];
      | None -> [])
      @
      [
        ("begin", `String begin_);
        ("end", `String end_);
        ("beginCaptures", captures syntax begin_captures);
        ("endCaptures", captures syntax end_captures);
        ("patterns", `List (List.map (fun reference -> `Assoc [("include", `String ("#" ^ reference))]) patterns))
      ]
  | Match {match_; match_name} -> 
    let match_, regexps  = List.split match_ in
    let match_ = List.map (fun f -> f.Core.textmate) match_ in
    let match_ = String.concat "" match_ in
    let _, captures_ = List.fold_left (fun (count, captures) cur ->
      match cur with 
        Some c -> (count + 1, (count + 1, c) :: captures)
      | None -> (count + 1, captures)
    ) (0, []) regexps in
    (match match_name with 
      Some s -> [("name", `String (highlight_to_textmate syntax s))]
    | None -> [])
    @
    [
      ("match", `String match_);      
      ("captures", captures syntax captures_)
    ] 
  
  and pattern syntax ({name; kind}: Core.pattern) = 
    `Assoc ([
      ("name", `String name);
    ] @ pattern_kind syntax kind)

  and repository syntax r : Yojson.Safe.t = 
    `Assoc (List.map (fun (i: Core.pattern) -> (i.name, `Assoc (pattern_kind syntax i.kind))) r)
    
  and language_features: Core.language_features -> Yojson.Safe.t = fun l ->
    `Assoc (
      comments l.comments
      @ brackets "brackets" l.brackets
      @ brackets "autoClosingPairs" l.auto_closing_pairs
      @ brackets "surroundingPairs" l.surrounding_pairs)

  and comments: Core.language_features_comments -> (string * Yojson.Safe.t) list = fun c ->
    [("comments", `Assoc (
      ["lineComment", `String c.line_comment.Core.textmate;
       "blockComment", `List [`String (fst c.block_comment).Core.textmate; `String (snd c.block_comment).Core.textmate]]
    ))]
    
    and brackets: string -> (string * string) list -> (string * Yojson.Safe.t) list = fun name l ->
      [(name, `List (List.map (fun b -> `List [`String (fst b); `String (snd b)]) l))]

  
  and to_yojson: string -> Core.t -> (Yojson.Safe.t * Yojson.Safe.t, Core.error) result = fun syntax s -> 
    ok @@ (`Assoc
    ((match s.folding_start_marker, s.folding_stop_marker with 
        Some folding_start_marker, Some folding_stop_marker -> [
          ("foldingStartMarker", `String folding_start_marker);
          ("foldingStopMarker", `String folding_stop_marker)
        ]
      | _, _ -> [])
    @
      [
      ("name", `String s.syntax_name);
      ("scopeName", `String s.scope_name);
      ("fileTypes", `List (List.map (fun s -> `String s) s.file_types));
      ("patterns", `List (List.map (fun reference -> `Assoc [("include", `String ("#" ^ reference))]) s.syntax_patterns));
      ("repository", repository syntax s.repository)
    ]),
    language_features s.language_features)

end

module Validate = struct

  let rec check_reference repository r =
    if r = "$self" then 
      ok true
    else 
      let exists = List.exists (fun (i: Core.pattern) -> i.name = r) repository in 
      if exists then 
        ok true
      else 
        error (Core.Referenced_rule_does_not_exist r)
        
  and pattern_kind name repository = function 
    Core.Begin_end {begin_; end_; patterns; _} -> 
      let begin_ = String.concat "" (List.map (fun f -> (fst f).Core.textmate) begin_) in
      let end_ = String.concat "" (List.map (fun f -> (fst f).Core.textmate) end_) in
      if String.trim begin_ = "" then 
        error (Core.Begin_cant_be_empty name)
      else if String.trim end_ = "" then
        error (Core.End_cant_be_empty name)
      else
        let patterns = List.fold_left (fun a c -> if is_error a then a else (
          check_reference repository c)) (ok true) patterns in
        fold ~ok ~error patterns
  | Match _m -> ok true  
  
  let syntax (s: Core.t) = 
    let repository = s.repository in
    let patterns = List.fold_left (fun a (c: Core.pattern) -> if is_error a then a else pattern_kind c.name repository c.kind) (ok true) repository in
    let curr = fold ~ok ~error patterns in
    let patterns = List.fold_left (fun a (c: string) -> if is_error a then a else check_reference repository c) curr s.syntax_patterns in
    fold ~ok ~error patterns

end

let add_comments s =
  let language_features = s.Core.language_features in
  let comments = language_features.comments in
  let line_comment = comments.line_comment in
  let block_comment = comments.block_comment in
  {s with 
    syntax_patterns = "block_comment" :: "line_comment" :: s.syntax_patterns;
    repository = 
      {
        name = "block_comment";
        kind = Begin_end {
          meta_name =      Some Core.Comment;
          begin_ =         [((fst block_comment), None)];
          end_ =           [((snd block_comment), None)];
          patterns =       [];
        }
      }
      ::
      {
        name = "line_comment";
        kind = Match {
          match_name = Some Core.Comment;
          match_ = [(line_comment, None)]
        }
      }
      :: s.repository
  }

let add_string s = 
  let language_features = s.Core.language_features in
  let string_delimiters = language_features.string_delimiters in
  let strings = List.map (fun d -> 
    Core.{
      name = "string";
      kind = Core.Begin_end {
        meta_name =      Some Core.String;
        begin_ =         [(d, None)];
        end_ =           [(d, None)];
        patterns =       [];
      }
    }
  ) string_delimiters 
  in
  {s with 
    syntax_patterns = "string" :: s.syntax_patterns;
    repository = strings @ s.repository
  }

let to_jsons: string -> Core.t -> (Yojson.Safe.t * Yojson.Safe.t, Core.error) result = fun syntax s ->
  let* _ = Validate.syntax s in
  let s = add_comments s in
  let s = add_string s in
  JSON.to_yojson syntax s
