{
  description = "ent project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs = { flake-parts, ... } @ inputs: flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      # ./module.nix
      # inputs.foo.flakeModule
    ];
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    perSystem = { config, self', inputs', pkgs, system, ... }: {
      packages.default = pkgs.callPackage ./ent.nix {
        trivialBuild = pkgs.emacs.pkgs.trivialBuild;
        seq = pkgs.emacsPackages.seq;
      };
      devShells.default = pkgs.mkShell {
        packages = [
          self'.packages.default
          # or      config.packages.default
        ];
      };
    };
  };
}
