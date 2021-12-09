{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          split
        ]);
in pkgs.mkShell {
  packages = [
    ghc
  ];
}