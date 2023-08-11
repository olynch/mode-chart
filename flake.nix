{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = hpkgs: [
          (hpkgs.callCabal2nix "mode-chart" ./. {})
        ];
        nativeBuildInputs = with pkgs; [
          cabal-install
          haskell-language-server
          hlint
          cabal2nix
          haskellPackages.implicit-hie
        ];
      };
    };
}
