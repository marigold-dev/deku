Printexc.record_backtrace true

let () = Test_increment.test ()

include Rely.Make (struct
  let config =
    Rely.TestFrameworkConfig.initialize
      { snapshotDir = "tests/__snapshots"; projectDir = "tests" }
end)
