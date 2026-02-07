{
  description = "com.nejla.common";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";  # or aarch64-darwin, etc.
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "912";
      haskell = pkgs.haskell.packages."ghc${ghcVersion}";
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.docker-compose
          pkgs.gnumake
          pkgs.go-task
          pkgs.postgresql_17
          pkgs.postgresql_17.pg_config
          pkgs.zlib

          haskell.ghc
          haskell.cabal-install
          haskell.hpack
          haskell.haskell-language-server
          haskell.ormolu
        ];
      };
    };
}
