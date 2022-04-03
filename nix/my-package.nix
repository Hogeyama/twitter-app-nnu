{ pkgs
, compiler
}:
let
  src = pkgs.lib.sourceByRegex ../. [
    "app"
    "app/.*"
    "lib"
    "lib/.*"
    "test"
    "test/.*"
    "conf"
    "conf/.*"
    "Setup.hs"
    "name-update.cabal"
    "README.md"
    "CHANGELOG.md"
    "LICENSE"
  ];
  haskPkgs = pkgs.haskell.packages.${compiler};
  myPkg = haskPkgs.callCabal2nix "name-update" src { };
  drv = myPkg.overrideAttrs (old: {
    checkPhase = ''
      set -eu
      dist/build/unit/unit
      dist/build/doctests/doctests
    '';

    installPhase = old.installPhase or "" + ''
      # Install config files
      install -d $out/conf
      install -m644 ${src}/conf/* $out/conf/
    '';

    fixupPhase = old.fixupPhase or "" + ''
      pushd $out

      # move executables
      mv ./bin/name-update-2434 ./
      rmdir ./bin

      # copy dynamic library
      mkdir -p ./lib
      cp $(ldd ./name-update-2434 | grep -F '=> ' | awk '{print $3}') ./lib

      # patch
      echo ${pkgs.patchelf}/bin/patchelf \
        --set-interpreter ./lib/ld-linux-x86-64.so.2 \
        --set-rpath ./lib \
        --force-rpath \
        ./
      ${pkgs.patchelf}/bin/patchelf \
        --set-interpreter ./lib/ld-linux-x86-64.so.2 \
        --set-rpath ./lib \
        --force-rpath \
        ./name-update-2434

      popd
    '';
  });
in
pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontHaddock drv)
