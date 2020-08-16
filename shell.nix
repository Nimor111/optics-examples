{pkgs ? import <nixpkgs> {}}:

let
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    lens
    stylish-haskell
  ];

  ghcWithLens = haskellPackages.ghcWithPackages haskellDeps;
in
pkgs.mkShell {
  buildInputs = [
    ghcWithLens

    pkgs.scala
    pkgs.sbt
    pkgs.jetbrains.idea-community
  ];
}
