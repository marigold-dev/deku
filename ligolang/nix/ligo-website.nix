{ buildNpmPackage, writeShellScriptBin, yarn, linkFarm, nodejs-slim, python2
, ligo-changelog }:
buildNpmPackage {
  src = ../gitlab-pages/website;
  npmBuild = "npm run build";
  preBuild = ''
    cp -r ${../gitlab-pages/docs} $NIX_BUILD_TOP/docs
    chmod 700 -R $NIX_BUILD_TOP/docs
    cp -f ${ligo-changelog}/changelog.md $NIX_BUILD_TOP/docs/intro/changelog.md
    cd versioned_docs && export LATEST_VERSION=$(ls -1r | sed -n '1p') && cd ..
    cp -f ${ligo-changelog}/changelog.md $NIX_BUILD_TOP/website/versioned_docs/$LATEST_VERSION/intro/changelog.md
  '';
  installPhase = ''
    cp -Lr build $out
    mkdir -p $out/deb
    cp ${../ligo.deb} $out/deb/ligo.deb
    mkdir -p $out/bin/linux
    cp -r ${../ligo} $out/bin/linux/ligo
    cp -r ${../doc} $out/odoc
  '';
  extraEnvVars.nativeBuildInputs = [ python2 ];
}
