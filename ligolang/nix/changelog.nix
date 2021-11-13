{ runCommand, writeTextFile, buildEnv, jq, yaml2json, git, mustache-go }:
let
  json = if builtins.elem ".git" (builtins.attrNames (builtins.readDir ../.)) then
    runCommand "changelog.json" { buildInputs = [ git jq yaml2json ]; } ''
      cp -r ${builtins.path { name = "git"; path = ../.git; }} .git
      cp -r ${../changelog} changelog
      cp -r ${../scripts} scripts
      bash ./scripts/changelog-json.sh > $out
    '' else builtins.toFile "changelog.json" (builtins.toJSON { changelog = [ { version = "Unknown; This executable was built without .git"; } ]; });

  changelog = {
    text = runCommand "changelog-text" { buildInputs = [ mustache-go ]; }
      "mkdir $out; mustache ${json} ${
        ../scripts/changelog.txt.mustache
      } > $out/changelog.txt";

    markdown =
      runCommand "changelog-markdown" { buildInputs = [ mustache-go ]; }
      "mkdir $out; mustache ${json} ${
        ../scripts/changelog.md.mustache
      } > $out/changelog.md";
  };

  release-notes-json = runCommand "release-notes.json" { buildInputs = [ jq ]; } ''
    jq '.changelog[0]' ${json} > "$out"
  '';

  release-notes = {
    text = runCommand "release-notes-text" { buildInputs = [ mustache-go ]; }
      "mkdir $out; mustache ${release-notes-json} ${
        ../scripts/release-notes.txt.mustache
      } > $out/release-notes.txt";

    markdown =
      runCommand "release-notes-markdown" { buildInputs = [ mustache-go ]; }
      "mkdir $out; mustache ${release-notes-json} ${
        ../scripts/release-notes.md.mustache
      } > $out/release-notes.md";
  };
in buildEnv {
  name = "changelogs";
  paths = [
    release-notes.text
    release-notes.markdown
    changelog.text
    changelog.markdown
  ];
}
