{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        haskellProjects.default = {
          devShell = {
            mkShellArgs = rec {
              buildInputs = with pkgs; [
                pkg-config
                gtk3
              ];

              LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
            };
          };
        };

        packages.default = self'.packages.Chart;
      };
    };
}
