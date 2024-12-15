{ pkgs, lib, config, inputs, ... }:

let
  ocamlPkgs = pkgs.ocaml-ng.ocamlPackages_5_1;
in
{
  packages = [ 
    pkgs.git
    ocamlPkgs.containers
    ocamlPkgs.iter
    ocamlPkgs.ppxlib
    ocamlPkgs.ppx_expect
    # profiling
    pkgs.linuxPackages_latest.perf
    pkgs.inferno
  ];

  languages.ocaml = {
    enable = true;
    packages = ocamlPkgs;
  };

  scripts.run-perf.exec = ''
    dune clean
    dune build --profile release
    perf record --call-graph dwarf -F 500 dune runtest --profile release
    perf script | inferno-collapse-perf | inferno-flamegraph > perf.flamegraph.svg
    echo "open perf.flamegraph.svg"
  '';

  pre-commit.hooks = {
    dune-fmt.enable = true;
  };

  pre-commit.hooks.tests = {
    enable = true;
    name = "build & tests";
    entry = "dune build @runtest";
    pass_filenames = false;
  };

  scripts.dev.exec = ''dune build @runtest --watch'';
}
