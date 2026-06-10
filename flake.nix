{
  description = "Haskell dev environment AtCoder";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        formatter = nixpkgs.legacyPackages.${system}.nixfmt-tree;

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.zsh
            (pkgs.haskell.packages.ghc9122.ghcWithPackages (ps: [
              ps.vector
              ps.containers
              ps.bytestring
              ps.unordered-containers
              ps.hashable
            ]))
            pkgs.haskell.packages.ghc9122.haskell-language-server
          ];
        };
      }
    );
}
