{
  description = "Comma-leading JSON formatter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages.default = import ./default.nix {
        pkgs = import nixpkgs { inherit system; };
      };
    });
}
