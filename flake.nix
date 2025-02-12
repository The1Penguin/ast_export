{
  description = "A Nix-flake-based Lean 4 development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        {
          packages.default = pkgs.clangStdenv.mkDerivation {
            name = "ast-export";
            src = self;
            buildPhase = "${pkgs.lean4}/bin/lake build";
            installPhase = "mkdir -p $out/bin; install -t $out/bin .lake/build/bin/ast-export";
          };

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              lean4
            ];
          };
        }
      );
}
