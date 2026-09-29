{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
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
  bigraph_builder =
    pkgs.ocamlPackages.buildDunePackage rec {
      pname = "bigraph";
      version = "2.0.0";
      duneVersion = "3";

      src = pkgs.lib.cleanSource ./vendor/bigraph-tools;

      buildInputs = [
        pkgs.boost
        pkgs.zlib

        pkgs.ocamlPackages.dune-configurator
        pkgs.ocamlPackages.ppx_deriving
        pkgs.ocamlPackages.ppx_deriving_yojson
        pkgs.ocamlPackages.yojson
        pkgs.ocamlPackages.zarith
      ];

      nativeBuildInputs = [
        pkgs.cmake
        pkgs.ocamlPackages.mdx
      ];

      dontUseCmakeConfigure = true;
    };

  TideScript_builder =
    pkgs.ocamlPackages.buildDunePackage rec {
      pname = "TideScript";
      version = "1.0.0";
      duneVersion = "3";

      src = pkgs.lib.cleanSourceWith {
        src = ./.;
        filter = path: type:
          let
            relativePath =
              pkgs.lib.removePrefix "${toString ./.}/" (toString path);
          in
          pkgs.lib.cleanSourceFilter path type
          && !(pkgs.lib.hasPrefix "_build" relativePath)
          && !(pkgs.lib.hasPrefix "vendor/bigraph-tools" relativePath);
      };

      buildInputs = [
        pkgs.boost
        pkgs.zlib

        pkgs.ocamlPackages.cmdliner
        pkgs.ocamlPackages.dune-configurator
        pkgs.ocamlPackages.menhirLib
        pkgs.ocamlPackages.mtime
        pkgs.ocamlPackages.ppx_deriving_yojson
        pkgs.ocamlPackages.ocamlgraph
        pkgs.ocamlPackages.progress
        pkgs.ocamlPackages.yojson
        pkgs.ocamlPackages.zarith
        bigraph_builder
      ];

      nativeBuildInputs = [
        pkgs.cmake
        pkgs.graphviz
        pkgs.obelisk

        pkgs.ocamlPackages.mdx
        pkgs.ocamlPackages.menhir
        pkgs.ocamlPackages.odoc
        pkgs.ocamlPackages.ocamlformat
        pkgs.ocamlPackages.dune_3
        pkgs.ocamlPackages.utop
        pkgs.ocamlPackages.merlin
        pkgs.emacsPackages.tuareg
        pkgs.ocamlPackages.ocp-indent
        pkgs.ocamlPackages.ounit2

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
  tidescript = TideScript_builder;

  shell = pkgs.mkShell {
    inputsFrom = [ tidescript ];
    OCAMLPATH = "";
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
