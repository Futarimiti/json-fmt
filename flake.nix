{
  description = "Comma-leading JSON formatter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        json-fmt = import ./default.nix { inherit pkgs; };
      in
      {
        packages.json-fmt = json-fmt;
        defaultPackage = json-fmt;
      }
    );
}
