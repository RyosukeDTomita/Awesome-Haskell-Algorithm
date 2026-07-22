{
  description = "Haskell dev environment AtCoder";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    # GitHub Actions メンテ用ツール(ghalint 等)は新しい nixpkgs から取る。
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        unstablePkgs = import nixpkgs-unstable { inherit system; };
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
      in
      {
        formatter = treefmtEval.config.build.wrapper;

        devShells.default = pkgs.mkShell {
          packages = [
            treefmtEval.config.build.wrapper
            pkgs.zsh
            (pkgs.haskell.packages.ghc9122.ghcWithPackages (ps: [
              ps.vector
              ps.containers
              ps.bytestring
              ps.unordered-containers
              ps.hashable
            ]))
            pkgs.haskell.packages.ghc9122.haskell-language-server
            # GitHub Actions のメンテ用ツール(配布物には含まれない)。
            unstablePkgs.pinact
            unstablePkgs.ghalint
          ];
        };
      }
    );
}
