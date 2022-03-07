Printexc.record_backtrace true
include Rely.Make (struct
  let config =
    Rely.TestFrameworkConfig.initialize
      { snapshotDir = "tests/__snapshots"; projectDir = "tests" }
end)
