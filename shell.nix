{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  packages = [
    pkgs.ocaml
    pkgs.opam
    pkgs.ocamlPackages.menhir
    pkgs.ocamlPackages.dune_3
    pkgs.ocamlPackages.utop
    pkgs.ocamlPackages.merlin
    pkgs.emacsPackages.tuareg
    pkgs.ocamlPackages.ocp-indent
    pkgs.ocamlPackages.ounit2
    pkgs.ocamlPackages.ocamlgraph
  ];
}
