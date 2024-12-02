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
  ];

  languages.ocaml = {
    enable = true;
    packages = ocamlPkgs;
  };

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
