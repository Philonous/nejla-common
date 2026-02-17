{
  description = "com.nejla.common";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "912";
      haskell = pkgs.haskell.packages."ghc${ghcVersion}";

      # Dependencies to build
      buildDeps = [
        pkgs.gnumake
        pkgs.go-task
        pkgs.postgresql_17
        pkgs.zlib
        pkgs.pkg-config
        haskell.ghc
        haskell.cabal-install
        haskell.hpack
      ];
      # Dependencies for local development
      devDeps  = [
        pkgs.docker-compose
        haskell.haskell-language-server
        haskell.ormolu
      ];
    in
      {
        devShells.${system} = {
          # Development shell
          default = pkgs.mkShell {
            buildInputs = buildDeps ++ devDeps;
            # Local dev probably has this set up anyway, but it doesn't hurt
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            LANG = "en_US.UTF-8";
          };
          # Shell for building in CI
          ci = pkgs.mkShell {
            buildInputs = buildDeps;
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            LANG = "en_US.UTF-8";
          };
        };
      };
}
