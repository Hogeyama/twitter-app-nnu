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
    "git-revision"
  ];

  # 参考: https://github.com/NixOS/nixpkgs/blob/6f7f655d46ed049c14329e3741c54a30b82a2a48/pkgs/development/haskell-modules/configuration-tensorflow.nix
  amazonka = pkgs.fetchFromGitHub {
    owner = "brendanhay";
    repo = "amazonka";
    rev = "0ccede621e56fb6f240e4850e205cde82d0e4a4b";
    sha256 = "0rs9bxxrw4wscf4a8yl776a8g880m5gcm75q06yx2cn3lw2b7v22";
    fetchSubmodules = true;
  };
  setAmazonkaSourceRoot = dir: drv:
    (pkgs.haskell.lib.overrideCabal drv (drv: { src = amazonka; })).overrideAttrs (_oldAttrs: { sourceRoot = "source/${dir}"; });
  # pkgs.haskell.lib.overrideCabal drv (drv: { src = amazonka; });

  haskPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      # test broken in ghc9.2
      type-errors = pkgs.haskell.lib.dontCheck super.type-errors;
      # test broken with time >= 0.10
      twitter-types = pkgs.haskell.lib.dontCheck (self.callHackage "twitter-types" "0.11.0" { });
      # 0.4.3 supports ghc9.2 for the first time.
      polysemy-plugin = self.callHackage "polysemy-plugin" "0.4.3.0" { };
      # polysemy-plugin requires polysemy>=1.7
      polysemy = self.callHackage "polysemy" "1.7.1.0" { };
      # https://github.com/brendanhay/amazonka/releases/tag/2.0.0-rc1 を使用する。
      amazonka-core = setAmazonkaSourceRoot "lib/amazonka-core" super.amazonka-core;
      amazonka-dynamodb = setAmazonkaSourceRoot "lib/services/amazonka-dynamodb" super.amazonka-dynamodb;

      # requirement for amazonka
      aeson = super.aeson_1_5_6_0; # => ビルド失敗
      # http-client = self.callHackage "http-client" "0.6.4.1" { };
    };
  };
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
