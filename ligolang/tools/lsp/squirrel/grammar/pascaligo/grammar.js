const common = require('../common.js')

let non_empty_injection = (Kind, element) =>
  choice(
    seq(
      Kind,
      common.sepEndBy1(';', element),
      'end',
    ),
    seq(
      Kind,
      '[',
      common.sepEndBy1(';', element),
      ']',
    ),
  )

let injection = (Kind, element) =>
  choice(
    seq(
      Kind,
      common.sepEndBy(';', element),
      'end',
    ),
    seq(
      Kind,
      '[',
      common.sepEndBy(';', element),
      ']',
    ),
  )

module.exports = grammar({
  name: 'PascaLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, /\s/],
  inline: $ => [$.parameters, $.arguments],

  rules: {
    source_file: $ => common.sepEndBy(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $.type_decl,
        $.const_decl,
        $.fun_decl,
        $.preprocessor,
      ),

    /// TYPE DECLARATION

    type_decl: $ =>
      seq(
        "type",
        field("typeName", $.TypeName),
        "is",
        field("typeValue", $._type_expr),
      ),

    _type_expr: $ => choice(
      $.sum_type,
      $.record_type,
      $._simple_type,
    ),

    // Sum type
    sum_type: $ => choice(
      common.sepBy1('|', field("variant", $.variant)),
      common.withAttrs($, seq('|', common.sepBy1('|', field("variant", $.variant)))),
    ),

    variant: $ => choice(
      common.withAttrs($, $._variant_simple),
      common.withAttrs($, $._variant_args),
    ),

    _variant_simple: $ => field("constructor", $.ConstrName),
    _variant_args: $ => seq(
      field("constructor", $.ConstrName),
      'of',
      field("arguments", $._simple_type)
    ),

    // Record type
    record_type: $ => common.withAttrs($,
      choice(
        seq('record', common.sepEndBy(';', field("field", $.field_decl)), 'end'),
        seq('record', '[', common.sepEndBy(';', field("field", $.field_decl)), ']'),
      ),
    ),

    field_decl: $ => common.withAttrs($,
      seq(
        field("fieldName", $.FieldName),
        ':',
        field("fieldType", $._type_expr),
      ),
    ),

    // Simple type
    /* upstream: `fun_type` -> `cartesian` -> `core_type` */
    _simple_type: $ => choice(
      $.Int,
      $.TypeName,
      $.TypeWildcard,
      $.fun_type,
      $.prod_type,
      $.app_type,
      $.michelsonTypeOr,
      $.michelsonTypeAnd,
      $.module_TypeName,
      $.type_group,
    ),

    module_TypeName: $ =>
      seq(
        common.sepBy1('.', field("path", $.ModuleName)),
        '.',
        field("type", $.TypeName),
      ),

    fun_type: $ => prec.right(1, seq(
      field("domain", $._simple_type),
      '->',
      field("codomain", $._simple_type),
    )),

    prod_type: $ => prec.right(2,
      seq(
        field("element", $._simple_type),
        '*',
        field("element", $._simple_type),
      )
    ),

    type_group: $ => common.par(field("type", $._type_expr)),

    app_type: $ => prec.left(8, seq(field("name", $._simple_type), $._type_arg)),

    _type_arg: $ => common.par(common.sepBy1(',', field("arg", $._type_expr))),

    michelsonTypeOr: $ =>
      seq(
        "michelson_or",
        "(",
        field("left_type", $._type_expr),
        ",",
        field("left_type_name", $.String),
        ",",
        field("right_type", $._type_expr),
        ",",
        field("right_type_name", $.String),
        ")",
      ),

    michelsonTypeAnd: $ =>
      seq(
        "michelson_pair",
        "(",
        field("left_type", $._type_expr),
        ",",
        field("left_type_name", $.String),
        ",",
        field("right_type", $._type_expr),
        ",",
        field("right_type_name", $.String),
        ")",
      ),

    /// CONSTANT DECLARATION

    const_decl: $ => common.withAttrs($,
      seq(
        'const',
        field("name", $.NameDecl),
        optional(seq(
          ':',
          field("type", $._type_expr),
        )),
        '=',
        field("value", $._expr),
      ),
    ),

    /// FUNCTION DECLARATION

    fun_decl: $ => common.withAttrs($,
      seq(
        field("recursive", optional($.recursive)),
        'function',
        field("name", $.NameDecl),
        $.parameters,
        optional(seq(':', field("type", $._type_expr))),
        'is',
        field("body", $._expr),
      ),
    ),

    parameters: $ => common.par(common.sepBy(';', field("parameter", $.param_decl))),

    _param_pattern: $ => choice(
      $.var_pattern,
      $.wildcard_pattern,
    ),

    param_decl: $ =>
      seq(
        field("access", $._access),
        field("name", $._param_pattern),
        ':',
        field("type", $._param_type),
      ),

    _access: $ => choice('var', 'const'),

    _param_type: $ => $._simple_type,

    /// STATEMENTS

    _statement: $ =>
      choice(
        $.type_decl,
        $._open_data_decl,
        $._instruction,
      ),

    _open_data_decl: $ =>
      choice(
        $.const_decl,
        $.var_decl,
        $.fun_decl,
      ),

    var_decl: $ =>
      seq(
        'var',
        field("name", $._pattern),
        optional(seq(':', field("type", $._type_expr))),
        ':=',
        field("value", $._expr),
      ),

    _instruction: $ =>
      choice(
        $.conditional,
        $.case_instr,
        $.assignment,
        $._loop,
        $.fun_call,
        $.projection_call,
        $.skip,
        $.record_patch,
        $.map_patch,
        $.set_patch,
        $.map_remove,
        $.set_remove,
      ),

    // Conditional Instruction
    conditional: $ =>
      seq(
        'if',
        field("selector", $._expr),
        'then',
        field("then", $._if_clause),
        optional(';'),
        'else',
        field("else", $._if_clause),
      ),

    _if_clause: $ =>
      choice(
        $._instruction,
        $.clause_block,
        $.block,
      ),

    clause_block: $ =>
      seq('{', common.sepEndBy1(';', field("statement", $._statement)), '}'),

    // Case Instruction
    case_instr: $ =>
      choice(
        seq(
          'case',
          field("subject", $._expr),
          'of',
          optional('|'),
          common.sepEndBy1('|', field("case", $.case_clause_instr)),
          'end'
        ),
        seq(
          'case',
          $._expr,
          'of',
          '[',
          optional('|'),
          common.sepEndBy1('|', field("case", $.case_clause_instr)),
          ']'
        ),
      ),

    case_clause_instr: $ =>
      seq(
        field("pattern", $._pattern),
        '->',
        field("body", $._if_clause),
      ),

    // Assignment
    assignment: $ =>
      seq(
        field("LHS", $._lhs),
        ':=',
        field("RHS", $._rhs),
      ),

    _lhs: $ => choice($._path, $.map_lookup),
    _rhs: $ => $._expr,

    // Loops
    _loop: $ => choice($.while_loop, $._for_loop),

    while_loop: $ =>
      seq(
        'while',
        field("breaker", $._expr),
        field("body", $.block),
      ),

    _for_loop: $ =>
      choice(
        $.for_cycle,
        $.for_box,
      ),

    for_cycle: $ =>
      seq(
        'for',
        field("name", $.Name),
        ':=',
        field("begin", $._rhs),
        'to',
        field("end", $._expr),
        optional(seq(
          "step",
          field("step", $._expr),
        )),
        field("body", $.block),
      ),

    for_box: $ =>
      seq(
        'for',
        field("key", $.Name),
        optional(seq('->', field("value", $.Name))),
        'in',
        field("kind", $.collection),
        field("collection", $._expr),
        field("body", $.block),
      ),

    collection: $ => choice('map', 'set', 'list'),

    // Function call
    fun_call: $ =>
      seq(
        field("f", choice($.Name)),
        $.arguments,
      ),

    // Projection call
    projection_call: $ => prec(1, seq(
      field("f", $._projection),
      $.arguments,
    )),

    // Record patch
    record_patch: $ =>
      seq(
        'patch',
        field("container", $._path),
        'with',
        non_empty_injection('record', field("binding", $.field_path_assignment)),
      ),

    // Map patch
    map_patch: $ =>
      seq(
        'patch',
        field("container", $._path),
        'with',
        non_empty_injection('map', field("binding", $.binding)),
      ),

    // Set patch
    set_patch: $ =>
      seq(
        'patch',
        field("container", $._path),
        'with',
        non_empty_injection('set', field("key", $._expr)),
      ),

    // Map remove
    map_remove: $ =>
      seq(
        'remove',
        field("key", $._expr),
        'from',
        'map',
        field("container", $._path),
      ),

    // Set remove
    set_remove: $ =>
      seq(
        'remove',
        field("key", $._expr),
        'from',
        'set',
        field("container", $._path),
      ),

    /// PATTERNS

    _pattern: $ =>
      choice(
        $.cons_pattern,
        $._core_pattern,
      ),

    cons_pattern: $ =>
      seq(
        field("head", $._core_pattern),
        '#',
        field("tail", $._pattern),
      ),

    wildcard_pattern: $ => "_",

    _core_pattern: $ =>
      choice(
        $.wildcard_pattern,
        $.Int,
        $.Nat,
        $.String,
        $._constr_pattern,
        $._list_pattern,
        $.record_pattern,
        $.tuple_pattern,
        $.var_pattern,
      ),

    // Constructor Pattern
    _constr_pattern: $ => choice(
      $.Unit,
      $.False,
      $.True,
      $.None,
      $.user_constr_pattern,
    ),

    user_constr_pattern: $ =>
      seq(
        field("constr", $.ConstrName),
        optional(field("arguments", $.tuple_pattern)),
      ),

    // List Pattern
    _list_pattern: $ =>
      choice(
        $.list_pattern,
        'nil',
      ),

    list_pattern: $ => injection("list", field("element", $._pattern)),

    // Record Pattern
    record_pattern: $ => injection("record", field("field", $._record_field_pattern)),

    _record_field_pattern: $ => choice(
      $.record_field_pattern,
      $.record_capture_pattern,
    ),

    record_field_pattern: $ => seq(
      field("name", $.FieldName),
      '=',
      field("body", $._core_pattern),
    ),

    record_capture_pattern: $ => field("name", $.NameDecl),

    // Tuple pattern
    tuple_pattern: $ =>
      common.par(common.sepBy1(',', field("element", $._pattern))),

    // Var pattern
    var_pattern: $ => field("name", $.NameDecl),

    /// EXPRESSIONS

    _expr: $ =>
      choice(
        $.case_expr,
        $.cond_expr,
        $.fun_expr,
        $.let_expr,
        $.michelson_interop,
        $._op_expr,
      ),

     // Case Expressions
     case_expr: $ =>
      choice(
        seq(
          'case',
          field("subject", $._expr),
          'of',
          optional('|'),
          common.sepEndBy1('|', field("case", $.case_clause_expr)),
          'end'
        ),
        seq(
          'case',
          field("subject", $._expr),
          'of',
          '[',
          optional('|'),
          common.sepEndBy1('|', field("case", $.case_clause_expr)),
          ']'
        ),
      ),

    case_clause_expr: $ =>
      seq(
        field("pattern", $._pattern),
        '->',
        field("body", $._expr),
      ),

    // Conditional expressions
    cond_expr: $ =>
      seq(
        'if',
        field("selector", $._expr),
        'then',
        field("then", $._expr),
        optional(';'),
        'else',
        field("else", $._expr),
      ),

    // Function Expressions
    fun_expr: $ =>
      seq(
        field("recursive", optional($.recursive)),
        'function',
        $.parameters,
        optional(seq(':', field("type", $._type_expr))),
        'is',
        field("body", $._expr),
      ),

    // Let expressions
    let_expr: $ =>
      seq(
        field("locals", $.block),
        'with',
        field("body", $._expr),
      ),

    // Michelson expressions
    michelson_interop: $ => seq(
      '[%Michelson',
      common.par(
        seq(
          field("code", $.michelson_code),
          ':',
          field("type", $._type_expr),
        )
      ),
      optional(common.par(common.sepBy(',', field("argument", $._expr)))),
      ']'
    ),

    michelson_code: $ => seq(
      '{|',
      repeat(/([^\|]|\|[^}])/),
      '|}'
    ),

    // Operation expressions
    _op_expr: $ =>
      choice(
        $.binop,
        $.unop,
        $._core_expr,
      ),

    binop: $ =>
      choice(
        prec.left(0, seq(field("arg1", $._op_expr), field("op", 'or'), field("arg2", $._op_expr))),
        prec.left(1, seq(field("arg1", $._op_expr), field("op", 'and'), field("arg2", $._op_expr))),
        prec.right(2, seq(field("arg1", $._core_expr), field("op", 'contains'), field("arg2", $._op_expr))),
        prec.left(3, seq(field("arg1", $._op_expr), field("op", $.comparison), field("arg2", $._op_expr))),
        prec.right(4, seq(field("arg1", $._op_expr), field("op", '^'), field("arg2", $._op_expr))),
        prec.right(5, seq(field("arg1", $._op_expr), field("op", '#'), field("arg2", $._op_expr))),
        prec.left(6, seq(field("arg1", $._op_expr), field("op", $.adder), field("arg2", $._op_expr))),
        prec.left(7, seq(field("arg1", $._op_expr), field("op", $.multiplier), field("arg2", $._op_expr))),
      ),

    comparison: $ => choice('<', '<=', '>', '>=', '=', '=/='),
    adder: $ => choice('-', '+'),
    multiplier: $ => choice('/', '*', 'mod'),

    unop: $ => prec.right(8, seq(field("negate", $.negate), field("arg", $._core_expr))),

    negate: $ => choice('-', 'not'),

    /// CORE EXPRESSIONS

    _core_expr: $ =>
      choice(
        $.Int,
        $.Nat,
        $.Tez,
        $.Name,
        $.String,
        $.Bytes,
        $.False,
        $.True,
        $.Unit,
        $.None,

        $.annot_expr,
        $.tuple_expr,
        $._list_expr,
        $._fun_call_or_par_or_projection,
        $._map_expr,
        $.set_expr,
        $.record_expr,
        $.update_record,
        $._constr_use,
        $.paren_expr,
      ),

    // Annotation expression
    annot_expr: $ =>
      common.par(seq(
        field("subject", $._op_expr),
        ':',
        field("type", $._type_expr)
      )),

    // Tuple expression
    tuple_expr: $ => common.par(seq(
      field("element", $._expr),
      ',',
      common.sepBy1(',', field("element", $._expr))),
    ),

    // List expression
    _list_expr: $ => choice($.list_injection, 'nil'),

    list_injection: $ => injection('list', field("element", $._expr)),

    // Function Call/Par Call/Projection
    _fun_call_or_par_or_projection: $ =>
      choice(
        $.par_call,
        $.projection_call,
        $.fun_call,
        $._projection,
      ),

    par_call: $ =>
      prec.right(1, seq(
        common.par(field("f", $._expr)),
        $.arguments,
      )),

    // Map Expression
    _map_expr: $ =>
      choice(
        $.map_lookup,
        $.map_injection,
        $.big_map_injection,
      ),

    map_lookup: $ =>
      seq(
        field("container", $._path),
        common.brackets(field("index", $._expr)),
      ),

    map_injection: $ => injection('map', field("binding", $.binding)),
    big_map_injection: $ => injection('big_map', field("binding", $.binding)),

    // Set Expression
    set_expr: $ => injection('set', field("element", $._expr)),

    // Record Expression
    record_expr: $ =>
      injection('record', field("assignment", $.field_path_assignment)),

    // Update Record Expression
    update_record: $ =>
      seq(
        field("record", $._path),
        'with',
        non_empty_injection('record', field("assignment", $.field_path_assignment)),
      ),

    // Constructor use expression
    _constr_use: $ =>
      choice(
        $.constr_call,
        $.ConstrName
      ),

    constr_call: $ =>
      seq(
        field("constr", $.ConstrName),
        $.arguments
      ),

    // Paren expression
    paren_expr: $ => common.par(field("expr", $._expr)),

    /// PROJECTION

    _projection: $ =>
      choice(
        $.data_projection,
        $.module_access,
      ),

    data_projection: $ => prec(11, seq(
      field("struct", $.Name),
      '.',
      $._accessor_chain,
    )),

    _accessor_chain: $ => prec.right(common.sepBy1('.', field("accessor", $._accessor))),
    _accessor: $ => choice($.FieldName, $.Int),

    module_access: $ => seq(
      common.sepBy1('.', field("path", $.ModuleName)),
      '.',
      field("field", $.FieldName),
    ),

    /// PREPROCESSOR

    // I (@heitor.toledo) decided to keep the preprocessors here since we still
    // attempt to parse the contract even if `ligo preprocess` failed.
    preprocessor: $ => field("preprocessor_command", choice(
      $.p_include,
      $.p_if,
      $.p_error,
      $.p_define,
    )),

    p_include: $ => seq(
      '#',
      'include',
      field("filename", $.String)
    ),

    p_import: $ => seq(
      '#',
      'import',
      field("filename", $.String),
      field("alias", $.String),
    ),

    p_if: $ => choice(
      seq(
        '#',
        choice('if', 'elif', 'else'),
        field("rest", $._till_newline),
      ),
      seq(
        '#',
        'endif',
      ),
    ),

    p_error: $ => seq('#', 'error', field("message", $._till_newline)),
    p_define: $ => seq('#', choice('define', 'undef'), field("definition", $._till_newline)),

    /// MISCELLANEOUS UTILITIES

    _path: $ => choice($.Name, $._projection),

    block: $ =>
      choice(
        seq(
          'begin',
          common.sepEndBy(';', field("statement", $._statement)),
          'end',
        ),
        seq(
          'block',
          '{',
          common.sepEndBy(';', field("statement", $._statement)),
          '}',
        ),
      ),

    binding: $ =>
     seq(
        field("key", $._expr),
        '->',
        field("value", $._expr),
      ),

    arguments: $ => common.par(common.sepBy(',', field("argument", $._expr))),

    field_path_assignment: $ =>
      seq(
        $._accessor_chain,
        '=',
        field("_rhs", $._expr),
      ),

    /// REGULAR EXPRESSIONS

    ConstrName: $ => $._NameCapital,
    FieldName: $ => $._Name,
    ModuleName: $ => $._NameCapital,
    TypeName: $ => $._Name,
    Name: $ => $._Name,
    NameDecl: $ => $._Name,

    _till_newline: $ => /[^\n]*\n/,

    attr: $ => /\[@[a-zA-Z][a-zA-Z0-9_:]*\]/,

    String: $ => choice(/\"(\\.|[^"])*\"/, /{\|(\\.|[^\|])*\|}/),
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    NameWildcard: $ => '_',
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,

    False: $ => 'False',
    True: $ => 'True',
    Unit: $ => 'Unit',
    None: $ => 'None',
    skip: $ => 'skip',
    recursive: $ => 'recursive',
  }
});
