Printexc.record_backtrace(true);
Mirage_crypto_rng_unix.initialize();
include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "tests/__snapshots",
      projectDir: "tests",
    });
});
