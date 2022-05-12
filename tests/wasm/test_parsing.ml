(* let code =
     {|(module
     (memory $mem 1)
     (export "memory" (memory $mem))
     (type $sum_t (func (param i32 i32) (result i32)))
     (func $sum_f (type $sum_t) (param $x i32) (param $y i32) (result i32)
       i32.const 0
       local.get $x
       local.get $y
       i32.add
       i32.store
       i32.const 0
       i32.load)
     (export "sum" (func $sum_f)))
   |}

   (* module Map = Map.Make(String)
      open Wasm.Script
      open Wasm.Source
      open Wasm
      let quote : script ref = ref []
      let scripts : script Map.t ref = ref Map.empty
      let modules : Ast.module_ Map.t ref = ref Map.empty
      let instances : Instance.module_inst Map.t ref = ref Map.empty
      let registry : Instance.module_inst Map.t ref = ref Map.empty

      let rec run_command cmd =

        match cmd.it with
        | Module (x_opt, def) ->
          quote := cmd :: !quote;
          let m = run_definition def in
          if not !Flags.unchecked then begin
            trace "Checking...";
            Valid.check_module m;
            if !Flags.print_sig then begin
              trace "Signature:";
              print_module x_opt m
            end
          end;
          bind scripts x_opt [cmd];
          bind modules x_opt m;
          if not !Flags.dry then begin
            trace "Initializing...";
            let imports = Import.link m in
            let inst = Eval.init m imports in
            bind instances x_opt inst
          end

        | Register (name, x_opt) ->
          quote := cmd :: !quote;
          if not !Flags.dry then begin
            trace ("Registering module \"" ^ Ast.string_of_name name ^ "\"...");
            let inst = lookup_instance x_opt cmd.at in
            registry := Map.add (Utf8.encode name) inst !registry;
            Import.register name (lookup_registry (Utf8.encode name))
          end

        | Action act ->
          quote := cmd :: !quote;
          if not !Flags.dry then begin
            let vs = run_action act in
            if vs <> [] then print_values vs
          end

        | Assertion ass ->
          quote := cmd :: !quote;
          if not !Flags.dry then begin
            run_assertion ass
          end

        | Meta cmd ->
          run_meta cmd *)
   let pparse () =
     let lexer = Lexing.from_string code in
     let parsed = Wasm.Parse.parse "code" lexer Wasm.Parse.Module in
     parsed

   let parse_test () =
     let _var_opt, def = pparse () in
     let instance =
       match def.it with
       | Wasm.Script.Textual m ->
         Wasm.Valid.check_module m;
         let imports = Wasm.Import.link m in
         let instance = Wasm.Eval.init m imports in
         instance
       | _ -> assert false in
     let _exported =
       Wasm.Instance.export instance (Wasm.Utf8.decode "sum") |> Option.get in
     let evaled =
       match _exported with
       | ExternFunc func ->
         Wasm.Eval.invoke func
           Wasm.I32.
             [
               Num (of_int_u 3 |> Wasm.Values.I32Num.to_num);
               Num (of_int_u 8

                |> Wasm.Values.I32Num.to_num);
             ]
       | _ -> assert false in
     print_endline @@ Wasm.Values.string_of_values evaled;
     Alcotest.(check' bool) ~msg:"uselss" ~expected:false ~actual:true
   let test_parsing =
     let open Alcotest in
     ( "Execution pattern errors",
       [test_case "Pattern 1 - Value should be pair" `Quick parse_test] ) *)
