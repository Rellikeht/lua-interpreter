{pkgs ? import <nixpkgs> {}}: let
  lua51 = pkgs.writeScriptBin "lua51" ''
    exec ${pkgs.lua51Packages.lua}/bin/lua $@
  '';
in
  pkgs.mkShell {
    pname = "lua";
    version = "5.1";
    src = ./.;

    nativeBuildInputs = with pkgs;
      [
      ]
      ++ [
        lua51
      ];
  }
