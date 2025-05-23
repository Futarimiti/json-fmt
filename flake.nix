{
  description = "Comma-leading JSON formatter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = rec {
          json-fmt = import ./default.nix { inherit pkgs; };
          default = json-fmt;
        };
        apps = rec {
          json-fmt = flake-utils.lib.mkApp { drv = self.packages.${system}.json-fmt; };
          default = json-fmt;
        };
      }
    );
}
