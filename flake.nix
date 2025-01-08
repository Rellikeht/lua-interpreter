{
  description = "Simple flake for building some program";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
    flakeUtils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flakeUtils,
  }: let
    systems = [
      "x86_64-linux"
      "i686-linux"

      "aarch64-linux"
      "armv7l-linux"

      "aarch64-darwin"
      "x86_64-darwin"
    ];
    flib = flakeUtils.lib;
  in
    flib.eachSystem systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      lib = pkgs.lib;
      name = "lua-interpreter";
      src = self;
    in {
      packages = {
        default = import ./default.nix {inherit pkgs;};
      };
    });
}
