include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "tests/__snapshots",
      projectDir: "tests",
    });
});
