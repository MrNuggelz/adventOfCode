{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          split
          matrix
        ]);
in pkgs.mkShell {
  packages = [
    ghc
  ];
}