{pkgs ? import <nixpkgs> {}}:
pkgs.ocamlPackages.buildDunePackage rec {
  pname = "lua_interpreter";
  version = "0.1";
  src = ./.;

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${pkgs.ocamlformat}
  '';

  buildInputs = with pkgs; ([
      #
    ]
    ++ (with ocamlPackages; [
      menhir
      base
      stdio
      ppx_show
      stdcompat
      ppxlib
      # ppx_deriving
      #
    ]));

  nativeBuildInputs =
    buildInputs
    ++ (with pkgs; ([
        #
      ]
      ++ (with ocamlPackages; [
        ocaml
        dune_3
        findlib
        utop

        ocaml-lsp
      ])));
}
