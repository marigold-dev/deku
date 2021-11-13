# LIGO Changelog

## [Unreleased](https://gitlab.com/ligolang/ligo/-/releases/Unreleased)
Run this release with Docker: `docker run ligolang/ligo:Unreleased`

* fixed: Testing framework: internal replace of compare function ([!1334 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1334))
* internal: Removed uses of vendor .opam in CI ([!1327 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1327))
* added: Docs: new features on mutation ([!1323 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1323))
* internal: Testing framework: add support for missing ops. on tez ([!1330 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1330))
* internal: Testing framework: add missing cases for iteration in JsLIGO (C_FOLD) ([!1328 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1328))
* internal: JsLIGO decompilation: quick fix for decompilation of expressions ([!1326 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1326))
* added: testing framework: adding timestamp arithmetic in LIGO interpreter ([!1322 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1322))

## [0.26.0](https://gitlab.com/ligolang/ligo/-/releases/0.26.0)
Run this release with Docker: `docker run ligolang/ligo:0.26.0`

* added: Add missing API functions to interpreter [List.fold_right, List.fold_left, List.head_opt, List.tail_opt, Set.fold_desc, Set.update, Map.get_and_update] ([!1319 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1319))
* internal: Add all dependencies to opam.ligo (including dependencies required by vendored dependencies) ([!1317 by Andre Popovitch](https://gitlab.com/ligolang/ligo/-/merge_requests/1317))
* internal: Use Dune for vendoring instead of Opam ([!1312 by Andre Popovitch](https://gitlab.com/ligolang/ligo/-/merge_requests/1312))
* fixed: fix typing error related to Test.to_contract primitive ([!1318 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1318))
* fixed: improves error locations on arguments in JsLigo ([!1315 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1315))
* changed: updating tezos dependencies to Granada (010-PtGRANAD) ([!1301 by Ulrik Strid](https://gitlab.com/ligolang/ligo/-/merge_requests/1301))
* removed: testing framework: deprecates bootstrapped accounts support due to a problem in one of our tezos dependency, this feature will be enabled again in further updates ([!1301 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1301))
* added: Testing framework: add steps bound (for timeout) ([!1308 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1308))
* fixed: JsLIGO: disallow variables with the same name in the same block to align with JS/TS ([!1296 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1296))
* added: Documentation of Preprocessor and LexerLib libraries. ([!1306 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1306))
* added: Testing framework: add attribute to mark non-mutable declarations ([!1303 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1303))
* internal: Generate manpages for new-cli & rename `ligo print preprocess` to `ligo print preprocessed` ([!1304 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1304))
* changed: Add assert_with_error funciton family ([!1300 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/1300))
* fixed: JsLIGO: improve floating block scope handling ([!1296 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1296))
* fixed: ReasonLIGO: support functions without arguments ([!1292 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1292))
* internal: Tests: remove unused warnings ([!1290 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1290))

## [0.25.0](https://gitlab.com/ligolang/ligo/-/releases/0.25.0)
Run this release with Docker: `docker run ligolang/ligo:0.25.0`

* fixed: Removed syntax of tupls without parentheses as parameters. ([!1299 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1299))
* fixed: Extended syntax to support 'let x (type a) : 'a = x' etc. ([!1299 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1299))
* internal: Change remove_unused of self_ast_typed to search for free variables in module experssions & free module variables in expressions ([!1295 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1295))
* changed: Remove JsLIGO new keyword ([!1293 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1293))
* internal: Refactor: use Combinators.equal_value to compare record, list, set & map in interpreter ([!1291 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1291))
* internal: Nested value comparison in interpreter for list, set & map ([!1274 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1274))
* internal: Testing framework: improve environment reconstruction (module support) ([!1289 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1289))
* internal: Testing framework: keep a local store for bigmaps ([!1285 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1285))
* performance: improve performance of get-scope --with-types option by re-using the type environment instead of building a partial program ([!1286 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1286))
* added: Testing framework: add support for modules ([!1280 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1280))
* fixed: Add support for chained assignment ([!1281 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1281))
* internal: Improve error message when type of contract can't be inferred ([!1268 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1268))
* changed: Modify Cli ([!1271 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/1271))

## [0.24.0](https://gitlab.com/ligolang/ligo/-/releases/0.24.0)
Run this release with Docker: `docker run ligolang/ligo:0.24.0`

* fixed: Fixed bug in preprocessing #import ([!1282 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1282))
* fixed: Fix bitwise operators in interpreter ([!1276 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1276))
* changed: Update to Florence 9.7 ([!1275 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/1275))
* fixed: Fix: Test.originate now supports recursive functions ([!1273 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1273))
* internal: Ast-typed pass: transform unused rec. to lambda ([!1254 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1254))
* internal: Testing framework: fix for constant declaration, it does not need to evaluate anymore ([!1269 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1269))
* internal: Docs: fix type signatures & examples for list, set & map ([!1257 by melwyn95](https://gitlab.com/ligolang/ligo/-/merge_requests/1257))
* fixed: JsLIGO: add support for tuple assignment ([!1262 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1262))
* internal: Testing framework: use ppx to check which operations correspond to it ([!1258 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1258))
* deprecated: Deprecates Test.compile_expression Test.compile_expression_subst Test.mutate_expression Test.mutate_count
 ([!1253 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1253))
* fixed: Fix: C_POLYMORPHIC_ADD resolution to C_CONCAT/C_ADD in JsLIGO ([!1256 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1256))
* fixed: Fix: annotations are no long uncapitalized ([!1241 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1241))
* added: Testing framework: add support for re-generating files from mutation ([!1214 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1214))

## [0.23.0](https://gitlab.com/ligolang/ligo/-/releases/0.23.0)
Run this release with Docker: `docker run ligolang/ligo:0.23.0`

* added: Sub-command compile-expression: add option for disabling running of compiled code ([!1252 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1252))
* internal: Fix: get_fv in self-ast-tyed/sef-ast-imperative ([!1248 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1248))
* fixed: Fix: failure in REPL's exception handling ([!1246 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1246))
* fixed: Fix: check that Bytes.unpack's annotated type is an option ([!1245 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1245))
* fixed: true/false in pattern matching are now forbidden (use True and False instead) ([!1179 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* fixed: Fix: remove unused declarations before compiling ([!1239 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1239))
* fixed: [PascaLIGO/CameLIGO] Rewrite of the parse error messages. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* fixed: [PascaLIGO] Fixed pretty-printing. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* removed: Removed preprocessing directives #region and #endregion. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* internal: [CameLIGO/ReasonLIGO/JsLIGO] Removed predefined constructors true and false. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* internal: [PascaLIGO] Removed predefined constructors Unit, True and False. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* internal: [PascaLIGO/CameLIGO/ReasonLIGO] Removed CST node TWild (standing for _). ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* added: Added attributes on lambdas. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* internal: Refactoring of the parsers, CSTs and printers. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* internal: JsLIGO: Fixed dangling else in the parser. Fixeds attributes on pattern variables and declarations. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* internal: Changed all Token.mll into Token.ml ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* added: Front-end: Parameteric type expressions, polymorphic type and value definitions. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* internal: All syntaxes: Removed predefined constructors Some and None. ([!1179 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* added: The typechecking now understand parametric types ([!1179 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1179))
* added: Testing framework: a call trace is kept for giving context on failure ([!1209 by er4333](https://gitlab.com/ligolang/ligo/-/merge_requests/1209))
* added: Add bitwise operators for CameLIGO & ReasonLIGO ([!1205 by Melwyn Saldanha](https://gitlab.com/ligolang/ligo/-/merge_requests/1205))
* fixed: Suspend tail recursion analysis in E_module_accessor ([!1192 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1192))

## [0.22.0](https://gitlab.com/ligolang/ligo/-/releases/0.22.0)
Run this release with Docker: `docker run ligolang/ligo:0.22.0`

* fixed: Fix: top-level underscore "definition" breaks testing framework ([!1238 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1238))
* internal: Handling errors with exception ([!1234 by pierre-emmanuel](https://gitlab.com/ligolang/ligo/-/merge_requests/1234))
* fixed: Testing framework: fix overriding of variables on environment reconstruction ([!1226 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1226))
* added: Contract pass: check entrypoint syntax in C_CONTRACT_ENTRYPOINT(_OPT) ([!1223 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1223))
* internal: Clean up error handling in testing framework ([!1218 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1218))
* fixed: Fix #1240: use annotations when checking Tezos.self ([!1217 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1217))

## [0.21.0](https://gitlab.com/ligolang/ligo/-/releases/0.21.0)
Run this release with Docker: `docker run ligolang/ligo:0.21.0`

* internal: Handling warnings as effect ([!1224 by pierre-emmanuel](https://gitlab.com/ligolang/ligo/-/merge_requests/1224))
* fixed: Trim warnings to not cause accidental error output. ([!1222 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1222))
* fixed: Fix editor extension page ([!1219 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1219))

## [0.20.0](https://gitlab.com/ligolang/ligo/-/releases/0.20.0)
Run this release with Docker: `docker run ligolang/ligo:0.20.0`

* added: Add commands for mutating CST/AST, and primitives formutation in testing framework ([!1156 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1156))
* changed: Improved ReasonLIGO syntax highlighting ([!1208 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1208))
* fixed: Transpiler: add parenthesis around typed pattern when decompiling CameLIGO ([!1204 by er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1204))
* fixed: Improve JsLIGO handling of attributes ([!1199 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1199))
* changed: JsLIGO: Don't require 'return' before 'failwith' ([!1198 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1198))
* fixed: Properly support tuple accessors when using '+' operator. ([!1197 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1197))

## [0.19.0](https://gitlab.com/ligolang/ligo/-/releases/0.19.0)
Run this release with Docker: `docker run ligolang/ligo:0.19.0`

* added: Add support for NEVER instruction as Tezos.never ([!1194 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1194))
* fixed: Fix type-checker bug when using let-destructuring with a unit pattern ([!1193 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1193))
* changed: Changes in the testing framework: Test.originate, Test.transfer(_exn), Test.to_contract, Test.to_entrypoint, Test.get_storage, Test.get_balance, Test.eval, initial support for big_maps ([!1169 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1169))
* fixed: REPL: fix evaluation of JsLIGO expressions and add test cases ([!1142 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1142))

## [0.18.0](https://gitlab.com/ligolang/ligo/-/releases/0.18.0)
Run this release with Docker: `docker run ligolang/ligo:0.18.0`

* fixed: fix a bug where Test.get_storage was not usable within a Test.compile_expression_subst ([!1182 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1182))
* internal: X_options maintenance ([!1155 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1155))
* internal: Improved review/refactor script (look for code quality marker, ignore tools/webide, show some info about why a file is at this position in the queue) ([!1136 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/1136))
* deprecated: Deprecated evaluate-value (now evaluate-expr) and run-function (now evaluate-call) ([!1131 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/1131))

## [0.17.0](https://gitlab.com/ligolang/ligo/-/releases/0.17.0)
Run this release with Docker: `docker run ligolang/ligo:0.17.0`

* internal: Rename Comments module to AttachComments ([!1178 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1178))
* fixed: Fixed typo's in tutorial and cheat sheet ([!1177 by @nicolas.van.phan](https://gitlab.com/ligolang/ligo/-/merge_requests/1177))
* fixed: Improve messages for var/const pass. ([!1174 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1174))
* added: New pass enforcing: consts cannot be assigned to, vars cannot be captured. ([!1132 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1132))
* fixed: Move decompilation of assign to self pass. Prepare pipeline for language specific decompilation ([!1172 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/1172))
* added: Added syntactic support for tuples without parentheses at the top-level of patterns in pattern matchings. ([!1168 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/1168))
* changed: Updating Taco-shop tutorial (all syntaxes, ligo test framework usage) ([!1152 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1152))
* fixed: fix a bug in ligo test framework where Test.transfer was returning unit in case of success ([!1164 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1164))
* added: Add warning message when layout attribute is present on a constructor for a sum (issue #1104) ([!1163 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1163))
* fixed: Restore earlier fix for lowercase switch cases ([!1161 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1161))
* fixed: Fix inversion in missmatch errors ([!1160 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/1160))
* fixed: Bugfix 2n/2 ([!1158 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/1158))
* changed: Improve JsLIGO lexer error messages ([!1159 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1159))
* changed: Adds a hint remembering users that warnings can be prevented using underscores ([!1154 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1154))
* changed: vendors/ligo-utils/simple-utils/x_list.ml: added code quality marker, some syntax nits ([!1151 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1151))

## [0.16.1](https://gitlab.com/ligolang/ligo/-/releases/0.16.1)
Run this release with Docker: `docker run ligolang/ligo:0.16.1`

_No changes for this version_

## [0.16.0](https://gitlab.com/ligolang/ligo/-/releases/0.16.0)
Run this release with Docker: `docker run ligolang/ligo:0.16.0`

* fixed: Fix issue with JsLIGO sequences ([!1150 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1150))
* fixed: Constructors without arguments are abstracted to constructor taking unit pattern ([!1148 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1148))
* internal: Removed ppx_let dependency, replaced by OCaml built-in let-operators ([!1147 by Rémi Lesénéchal and galfour](https://gitlab.com/ligolang/ligo/-/merge_requests/1147))
* fixed: fix dry-run rejecting Tezos.self ([!1133 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1133))
* changed: src/test/test_helpers.ml: added code quality marker, added helper for get_program, use it in other test files ([!1138 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1138))
* added: Add more documentation on LIGO testing framework ([!1128 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1128))
* added: Pass for repeated usage of ticket values ([!1065 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1065))
* added: added variable references support (get-scope) ([!1101 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1101))

## [0.15.0](https://gitlab.com/ligolang/ligo/-/releases/0.15.0)
Run this release with Docker: `docker run ligolang/ligo:0.15.0`

* internal: Script which shows a queue of files that need refactoring ([!1105 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/1105))
* internal: Preparation work for merging type_value and type_expression ([!1109 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/1109))
* internal: rename inferance → inference ([!1108 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/1108))
* internal: Clean up comments in the tests from debugging the typer ([!1106 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/1106))
* fixed: Fix JsLIGO identifiers to include _ ([!1104 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1104))
* fixed: Fix failure for TWild (_ for types) ([!1103 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1103))
* removed: Remove unused warning message for rec. definitions ([!1111 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1111))
* fixed: Fix order in which things are evaluated in evaluate-value ([!1087 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1087))
* fixed: Fix failure when applying a type var. not expecting arguments ([!1110 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1110))
* internal: More testing ability in the documentation ([!1093 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1093))
* fixed: Fix support for lowcase verbatim strings in JsLIGO (important for test framework support) ([!1120 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1120))
* performance: Added compile-time optimization of comb record destructuring and uncurrying ([!1102 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1102))
* added: Compiler now emits Edo GET k and UPDATE k when [@layout:comb] is used with records ([!1102 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1102))
* fixed: Add JsLIGO to Edo features documentation ([!1116 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1116))
* fixed: Fix JsLIGO sapling types ([!1116 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1116))

## [0.14.0](https://gitlab.com/ligolang/ligo/-/releases/0.14.0)
Run this release with Docker: `docker run ligolang/ligo:0.14.0`

* added: Add type inference to the compiler ([!1026 by @ligo.suzanne.soy, @pewulfman, @lesenechal.remi](https://gitlab.com/ligolang/ligo/-/merge_requests/1026))
* added: LIGO Test Framework documentation ([!1092 by @SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/1092))
* added: Support for deep pattern, record pattern, wildcard ("_") pattern and unit pattern in matching expressions ([!934 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/934))
* added: Allow identifiers with underscore and wildcard name ([!1078 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1078))
* added: Add description and documentation for each sub command ([!1028 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1028))
* fixed: fix bug occuring when overriding option type ([!1082 by Rémi lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1082))
* internal: Add lexical units ([!919 by @rinderkn](https://gitlab.com/ligolang/ligo/-/merge_requests/919))
* added: Add support for JsLIGO ([!896 by @SanderSpies, @rinderkn](https://gitlab.com/ligolang/ligo/-/merge_requests/896))
* internal: Support for tuples without parentheses as last expressions in sequences. ([!1064 by @rinderkn](https://gitlab.com/ligolang/ligo/-/merge_requests/1064))
* internal: Fix typing of For_each with any type ([!1033 by @pewulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/1033))
* added: Explain sequences in ReasonLIGO and CameLIGO ([!1062 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1062))
* added: Add werror flag to mark warnings as errors ([!1060 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1060))
* internal: Uncurry before inlining ([!1056 by @tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1056))
* added: Add repl ([!1001 by @er433](https://gitlab.com/ligolang/ligo/-/merge_requests/1001))

## [0.13.0](https://gitlab.com/ligolang/ligo/-/releases/0.13.0)
Run this release with Docker: `docker run ligolang/ligo:0.13.0`

* fixed: Changed colour of background and foreground for hovered items in contact page, fixes #215 ([!1059 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/1059))
* fixed: fixing nested record update bug by normalizing Edo combs decompilation ([!1047 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1047))
* added: Add a REPL based on linenoise ([!1001 by Exequiel Rivas](https://gitlab.com/ligolang/ligo/-/merge_requests/1001))

## [0.12.0](https://gitlab.com/ligolang/ligo/-/releases/0.12.0)
Run this release with Docker: `docker run ligolang/ligo:0.12.0`

* added: The Emacs ligo-mode is now released on MELPA ([!1008 by sanityinc](https://gitlab.com/ligolang/ligo/-/merge_requests/1008))
* removed: Dropped support for pre-Edo protocols ([!1025 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1025))
* added: Some optimizations for Edo, including DUP n ([!1017 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1017))
* fixed: Tezos.self_address is now allowed in lambdas ([!1035 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1035))
* added: [@layout:comb] record destructuring is now compiled to UNPAIR n ([!1030 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1030))
* fixed: Curried functions again work correctly with commands like interpret and compile-storage ([!1038 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1038))
* removed: Dropped support for pre-Edo protocols (carthage, dalphanet) ([!1025 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/1025))

## [0.11.0](https://gitlab.com/ligolang/ligo/-/releases/0.11.0)
Run this release with Docker: `docker run ligolang/ligo:0.11.0`

* added: add set update primitive ([!1021 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/1021))
* added: prototype typer: separated typeclass deduce_and_clean to its own heuristic; trimmed down the Compat modules ([!981 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/981))
* added: prototype typer: heuristic to inline the definition of type variables used in type classes ([!981 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/981))
* added: Fixed potential bug: use multiset in typeclasses_constraining; added indexer for typeclasses using a variable as an unbound var; sketched out heuristic which inlines variabes in typeclasses ([!981 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/981))

## [0.10.0](https://gitlab.com/ligolang/ligo/-/releases/0.10.0)
Run this release with Docker: `docker run ligolang/ligo:0.10.0`

* changed: Prevent inappropriate optimisation of %Michelson lambdas ([!965 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/965))
* internal: Refactor & generalize Michelson peephole framework ([!966 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/966))
* fixed: Fix handling of true/false constructors in switch expressions. ([!987 by Sander Spies](https://gitlab.com/ligolang/ligo/-/merge_requests/987))
* internal: Use `opam lock` to lock dependencies ([!957 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/957))
* internal: Progress on modular interface for heuristics and abstraction over type_variable, renamed modules, moved files ([!972 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/972))
* internal: Refactoring of the front-end. ([!751 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/751))
* internal: Fixed the CST printers. ([!916 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/916))
* fixed: Added parsing of constant constructors as function arguments. ([!951 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/951))
* added: Uncurry functions only used in full applications ([!922 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/922))
* added: Few fixes in tickets untranspilation ([!929 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/929))
* added: Added doc for Edo things (tickets/sapling) ([!929 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/929))
* changed: ligo michelson backend now uses the "protocol-*-008-PtEdoTez-*" ([!926 by Rémi Lesénéchal, Tom Jack](https://gitlab.com/ligolang/ligo/-/merge_requests/926))
* changed: Upgrade tezos backend (for dry-run, compile-expression..) to Edo ([!926 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/926))

## [0.9.0](https://gitlab.com/ligolang/ligo/-/releases/0.9.0)
Run this release with Docker: `docker run ligolang/ligo:0.9.0`

* added: Add chain_id literals ([!953 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/953))
* fixed: use letters instead of numbers for type variables in debug trace ([!918 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/918))
* fixed: Fixed small bug in grouped_by_variable (use a multiset instead of a set, so that double-add and removal don't remove both copies ([!915 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/915))
* fixed: Fix assert in type_and_subst ([!915 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/915))
* internal: Addded pretty-printers to the API of indexer plug-ins; added test for addition of constraints and merging of type variables for the by_constraint_identifier indexer and other indexers. ([!914 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/914))

## [0.8.0](https://gitlab.com/ligolang/ligo/-/releases/0.8.0)
Run this release with Docker: `docker run ligolang/ligo:0.8.0`

* fixed: ReasonLIGO: Allow if without else in more places. ([!935 by Sander Spies](https://gitlab.com/ligolang/ligo/-/merge_requests/935))

## [0.7.1](https://gitlab.com/ligolang/ligo/-/releases/0.7.1)
Run this release with Docker: `docker run ligolang/ligo:0.7.1`

* internal: Improve transpilation and speed up transpilation tests ([!927 by Sander Spies](https://gitlab.com/ligolang/ligo/-/merge_requests/927))

## [0.7.0](https://gitlab.com/ligolang/ligo/-/releases/0.7.0)
Run this release with Docker: `docker run ligolang/ligo:0.7.0`

* internal: Remove vendored protocols -- fake origination & bake for proto env setup ([!921 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/921))
* added: tuple destructuring are now transformed into nested pattern matches ([!909 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/909))
* added: linear pattern matching (tuple/record) ([!909 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/909))
* added: EDO primitives (use --protocol edo and --disable-michelson-typechecking) ([!909 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/909))

## [0.6.0](https://gitlab.com/ligolang/ligo/-/releases/0.6.0)
Run this release with Docker: `docker run ligolang/ligo:0.6.0`

* added: prototype typer: added a separate constraint checking routine which will be executed after type inference ([!910 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/910))
* internal: prototype typer: split DB index tests into separate files, added test skeleton for cycle_detection_topological_sort ([!910 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/910))
* fixed: Fix bug with let_in annotations in recursive functions ([!905 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/905))
* fixed: the CLI print-* commands now print proper JSON when --format=json, also removed the sugar field from the core representation ([!892 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/892))
* changed: Use virtual ES6FUN token for ReasonLIGO to allow for more accurate error messages. ([!876 by Sander Spies](https://gitlab.com/ligolang/ligo/-/merge_requests/876))
* internal: Add module_access expression and type_expression ([!871 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/871))
* internal: Compile a multiple file contract ([!867 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/867))

## [0.5.0](https://gitlab.com/ligolang/ligo/-/releases/0.5.0)
Run this release with Docker: `docker run ligolang/ligo:0.5.0`

* changed: Add missing location to parser error messages in JSON ([!840 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/840))
* added: Add two new function:
- List.head_opt
- List.tail_opt

which returns none if the list is empty and respectively hd and tl if the list is CONS (hd,tl) ([!887 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/887))
* changed: Change in compilation of record updates ([!848 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/848))
* changed: Port stacking pass to Coq ([!848 by tomjack](https://gitlab.com/ligolang/ligo/-/merge_requests/848))
* internal: Add a build system that type file in order to resolve dependency ([!854 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/854))

## [0.4.0](https://gitlab.com/ligolang/ligo/-/releases/0.4.0)
Run this release with Docker: `docker run ligolang/ligo:0.4.0`

* added: alias_selector for heuristics to take into account aliases ([!839 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/839))
* fixed: Unprotected Sys.getenv ([!839 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/839))
* internal: Disable failing tests for typer not currently in use when it is in use (except the one being worked on) ([!839 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/839))
* internal: Fixed build issue with dune b inside a nix-shell (thanks Sander for providing the fix) ([!839 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/839))
* internal: scripts/add-changelog-entry.sh now runs a nested nix-shell if jq or json2yaml aren't installed in the outer nix shell ([!839 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/839))
* internal: boolean constant to force use of the typer which is not currently in use in the tests ([!839 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/839))
* internal: Solve warnings during compilation ([!871 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/871))
* added: internal documentation for the typer ([!866 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/866))
* added: add --typer option to choose between 'old' and 'new' typer ([!864 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/864))

## [0.3.0](https://gitlab.com/ligolang/ligo/-/releases/0.3.0)
Run this release with Docker: `docker run ligolang/ligo:0.3.0`

* internal: Create polymorphic function and use them in asts and passes to factorize code and simplify maintenance ([!770 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/770))
* fixed: Fix broken Nix build on MacOS ([!852 by Alexander Bantyev](https://gitlab.com/ligolang/ligo/-/merge_requests/852))
* fixed: Documentation of Pascaligo: change deprecated bytes_unpack to Bytes.unpack. ([!820 by Sander Spies](https://gitlab.com/ligolang/ligo/-/merge_requests/820))
* performance: Optimization: push DROP into failing conditionals ([!820 by Tom](https://gitlab.com/ligolang/ligo/-/merge_requests/820))
* internal: Refactoring of front-end (making libraries, removing exceptions from interfaces, factoring and removing code etc.) ([!751 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/751))
* fixed: Fixed 2 bugs in changelog generator (a shell variable wasn't exported but was used in a subprocess; the evaluation time of some nix expressions seem to have changed so we're invoking a shell command instead of having the nix expression evaluated before its inputs are ready) ([!853 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/853))
* internal: Move the preprocessor away from the parser ([!851 by Pierre-Emmanuel Wulfman](https://gitlab.com/ligolang/ligo/-/merge_requests/851))
* added: --protocol preloads types corresponding to a given protocol. use "ligo compile-contract path entrypoint --protocol=X --disable-michelson-typecheking" in combination with michelson_insertion ([!837 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/837))
* changed: Type constant are now loaded into the type environment, they become variables until the typed AST ([!849 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/849))
* changed: In the typer which is not currently in use, added a boolean flag on constraints indicating whether they might be necessary for correctness ([!814 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/814))
* fixed: Fix broken build after removal of generated code ([!807 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/807))
* :  ([! by ](https://gitlab.com/ligolang/ligo/-/merge_requests/))
* changed: In the typer which is not currently in use, constraint removal is now handled by normalizers ([!801 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/801))
* changed: In the typer which is not currently in use, heuristics can now request the justified removal of some of the constraints ([!801 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/801))

## [0.2.1](https://gitlab.com/ligolang/ligo/-/releases/0.2.1)
Run this release with Docker: `docker run ligolang/ligo:0.2.1`

* added: --lib option for the get-scope command. this allows to specify paths to included files ([!827 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/827))
* fixed: fixed a but in new michelson layout annotation ([!833 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/833))
* fixed: add the --init-file option to compile-expresssion command ([!828 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/828))

## [0.2.0](https://gitlab.com/ligolang/ligo/-/releases/0.2.0)
Run this release with Docker: `docker run ligolang/ligo:0.2.0`

* fixed: Fix error messages snippet printing ([!824 by SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/824))
* changed: Error messages improved. Now shows a code snippet and error message footer removed. ([!813 by SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/813))
* added: two new attributes on record and variant types: annot for michelson field annotation and layout (comb or tree) for compilation layout ([!809 by Rémi lesénéchal & Gabriel Alfour & Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/809))
* fixed: Fix wrong error message due to wrong merge ([!803 by SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/803))
* fixed: Redeploy website on release to update the changelog ([!804 by balsoft](https://gitlab.com/ligolang/ligo/-/merge_requests/804))
* changed: Added dependency on json2yaml and jq in nix-shell ([!800 by Suzanne Soy](https://gitlab.com/ligolang/ligo/-/merge_requests/800))

## [0.1.0](https://gitlab.com/ligolang/ligo/-/releases/0.1.0)
Run this release with Docker: `docker run ligolang/ligo:0.1.0`

* added: Add a dialect option when decompiling to pascaligo ([!791 by Alexandre Moine](https://gitlab.com/ligolang/ligo/-/merge_requests/791))
* fixed: Fixed error message when lexing an odd-lengthed byte sequence. ([!786 by Christian Rinderknecht](https://gitlab.com/ligolang/ligo/-/merge_requests/786))
* changed: Removed dependencies on the Tezos protocol from compiler stages and passes. ([!783 by Tom Jack](https://gitlab.com/ligolang/ligo/-/merge_requests/783))
* added: Add a predifine function 'assert_some' ([!775 by Pierre-Emmanuel](https://gitlab.com/ligolang/ligo/-/merge_requests/775))
* fixed: Add locations to the constant typers for all the primitives ([!773 by Rémi Lesénéchal](https://gitlab.com/ligolang/ligo/-/merge_requests/773))
* added: Add error message for function argument tuple components mismatch ([!772 by SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/772))
* added: Add missing function argument type annotation error ([!769 by SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/769))
* changed: Improve non-parser error messages ([!738 by SanderSpies](https://gitlab.com/ligolang/ligo/-/merge_requests/738))
* fixed: dune runtest in nix-shell ([!741 by balsoft](https://gitlab.com/ligolang/ligo/-/merge_requests/741))

## [0.0.1](https://gitlab.com/ligolang/ligo/-/releases/0.0.1)
Run this release with Docker: `docker run ligolang/ligo:0.0.1`

* added: Add changelog and versioning ([!725 by balsoft](https://gitlab.com/ligolang/ligo/-/merge_requests/725))

## [0.0.0](https://gitlab.com/ligolang/ligo/-/releases/0.0.0)
Run this release with Docker: `docker run ligolang/ligo:0.0.0`

_No changes for this version_

