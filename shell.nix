{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "com.nejla.app";
  buildInputs= [
    gnumake
    docker-compose

    # haskellPackages.hindent
  ];


}
