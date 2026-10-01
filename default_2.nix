{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/25.11.tar.gz") {
  overlays = [
    (final: prev: {
      ocamlPackages = prev.ocamlPackages.overrideScope (self: super: {
        # mdx introduces a second conflicting logs so we force it to be the same
        # as the rest of the set
        mdx = super.mdx.override {
          logs = self.logs;
        };
      });
    })
  ];
} }:

let
  tidescript_builder =
    pkgs.ocamlPackages.buildDunePackage rec {
      pname = "tideScript";
      version = "1.0.0";
      duneVersion = "3";

      src = pkgs.lib.cleanSource ./.;

      buildInputs = [
        pkgs.boost
        pkgs.zlib

        pkgs.ocamlPackages.cmdliner
        pkgs.ocamlPackages.dune-configurator
        pkgs.ocamlPackages.mtime
        pkgs.ocamlPackages.ppx_deriving_yojson
        pkgs.ocamlPackages.progress
        pkgs.ocamlPackages.yojson
        pkgs.ocamlPackages.zarith
        pkgs.ocamlPackages.menhir
        #pkgs.ocamlPackages.tuareg
        pkgs.ocamlPackages.ocamlgraph
        pkgs.ocamlPackages.ounit2
        pkgs.ocamlPackages.utop
      ];

      nativeBuildInputs = [
        pkgs.cmake
        pkgs.graphviz
        pkgs.obelisk

        pkgs.ocamlPackages.mdx
        pkgs.ocamlPackages.menhir
      ];

      # Cmake is used for GBS but handled by dune so ignore this
      dontUseCmakeConfigure = true;

      buildPhase = ''
          runHook preBuild
          dune build @install --profile=release -j $NIX_BUILD_CORES
          runHook postBuild
        '';

      installPhase = ''
          runHook preInstall
          dune install --profile=release --prefix=$out -j $NIX_BUILD_CORES
          runHook postInstall
        '';
    };
in
rec {
  tidescript = tidescript_builder;

  shell = pkgs.mkShell {
    inputsFrom = [ tidescript ];
    buildInputs = [
      pkgs.ocamlPackages.ocamlformat
      pkgs.git # Needed for CI
    ];
  };

  tidescript_docker =
    pkgs.dockerTools.buildImage {
      name = "tidescript";
      tag = "latest";
      copyToRoot = tidescript;
      config = {
        Cmd = [ "/bin/tidescript" ];
        WorkingDir = "/data";
      };
    };
}
