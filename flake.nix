{
  description = "srid/heist-extra: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";

    heist.url = "github:snapframework/heist"; # Waiting for 1.1.1.0 on nixpkgs cabal hashes
    heist.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          packages.heist-extra.root = ./.;
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
          hlintCheck.enable = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
      };

      # CI configuration
      flake.herculesCI.ciSystems = [ "x86_64-linux" "aarch64-darwin" ];
    };
}
