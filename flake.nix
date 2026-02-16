{
  description = "Home Manager configuration for sepeth";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      mkHome = system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./home.nix
          ];
        };
    in
    {
      homeConfigurations = {
        sepeth-darwin = mkHome "aarch64-darwin";
        sepeth-linux = mkHome "x86_64-linux";
        sepeth-linux-arm = mkHome "aarch64-linux";
      };
    };
}
