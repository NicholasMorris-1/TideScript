{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    ocaml
    opam
    ocamlPackages.menhir
    ocamlPackages.dune_3
    ocamlPackages.utop
    ocamlPackages.merlin
    emacsPackages.tuareg
    ocamlPackages.ocp-indent
    ocamlPackages.ocamlformat_0_26_0



  ];
}
