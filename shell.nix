{pkgs ? import <nixpkgs> {}}:

let
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    lens
    stylish-haskell
  ];

  ghcWithLens = haskellPackages.ghcWithPackages haskellDeps;
  scalaDeps = [
    pkgs.scala
    pkgs.sbt
    pkgs.jetbrains.idea-community
  ];
in
pkgs.mkShell {
  buildInputs = [
    ghcWithLens
    scalaDeps
  ];
}
