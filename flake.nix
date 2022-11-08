{
  description = "TODO";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils, ... }:
    let
      compiler = "ghc8107";
      supportedSystems = [ "x86_64-linux" ];

    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        outputs-overlay = pkgs: prev: {
          # unstable packages are available as pkgs.unstable.${package}
          unstable = nixpkgs-unstable.outputs.legacyPackages.${system};
          my-package = import ./nix/my-package.nix { inherit pkgs compiler; };
          my-shell = import ./nix/my-shell.nix { inherit pkgs compiler; };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ outputs-overlay ];
        };
      in
      {
        defaultPackage = pkgs.my-package;
        devShell = pkgs.my-shell;
      }
    );
}
