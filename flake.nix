{
  description = "srid/heist-extra: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
    check-flake.url = "github:srid/check-flake";

    heist.url = "github:srid/heist/expose-tpath";
    heist.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
        inputs.check-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
          } // config.treefmt.formatters;
          source-overrides = {
            inherit (inputs) heist;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            heist = dontCheck super.heist; # Tests are broken.
          };
          enableHLSCheck = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
      };
    };
}
