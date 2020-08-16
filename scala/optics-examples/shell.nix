{pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  name = "optics-examples-shell";
  buildInputs = [
    pkgs.scala
    pkgs.sbt
    pkgs.jetbrains.idea-community
  ];
}
