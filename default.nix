{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz") {} }:

let
  ocamlPackages = pkgs.ocamlPackages;
  bigraphTools = import ./vendor/bigraph-tools/default.nix { inherit pkgs; };

  tideScript = pkgs.stdenv.mkDerivation {
    pname = "tidescript";
    version = "0.1.0";
    src = pkgs.lib.cleanSourceWith {
      src = ./.;
      filter = path: type:
        let relativePath = pkgs.lib.removePrefix "${toString ./.}/" (toString path);
        in !pkgs.lib.hasPrefix "vendor" relativePath;
    };

    nativeBuildInputs = [
      ocamlPackages.dune_3
      ocamlPackages.ocaml
      ocamlPackages.findlib
      ocamlPackages.menhir
      pkgs.git
    ];

    buildInputs = [
      ocamlPackages.ocamlgraph
      ocamlPackages.ounit2
    ];

    dontConfigure = true;
    buildPhase = "dune build --profile=release @install @runtest";
    installPhase = "dune install --profile=release --prefix=$out";
  };
in
{
  inherit tideScript;
  bigrapher = bigraphTools.bigrapher;
  bigraphTools = bigraphTools.bigrapher;

  shell = pkgs.mkShell {
    packages = [
      tideScript
      bigraphTools.bigrapher
      pkgs.opam
      pkgs.git
      pkgs.cmake
      pkgs.boost
      pkgs.zlib
      pkgs.graphviz
      ocamlPackages.ocaml
      ocamlPackages.findlib
      ocamlPackages.dune_3
      ocamlPackages.menhir
      ocamlPackages.ocamlformat
      ocamlPackages.ocamlgraph
      ocamlPackages.ounit2
      ocamlPackages.cmdliner
      ocamlPackages.mtime
      ocamlPackages.progress
      ocamlPackages.zarith
      ocamlPackages.dune-configurator
      ocamlPackages.yojson
      ocamlPackages.ppx_yojson_conv
      ocamlPackages.ocp-indent
      pkgs.emacsPackages.tuareg
    ];
  };
}
