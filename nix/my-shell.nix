{ pkgs
, compiler
}:
let
  # ghcXYZ→XYZ
  compiler-version =
    builtins.substring 3 (builtins.stringLength compiler) compiler;
  haskPkgs = pkgs.haskell.packages.${compiler};
  shell = haskPkgs.shellFor {
    withHoogle = true;
    packages = _: [ pkgs.my-package ];
    buildInputs = with pkgs; [
      nixfmt
      cabal-install
      haskellPackages.cabal-fmt
      haskellPackages.fourmolu
      haskellPackages.hlint
      (haskell-language-server.override {
        supportedGhcVersions = [ compiler-version ];
      })
      # 新しいバージョンを使う
      unstable.terraform
    ];
  };
in
shell
