{
  description = "An implementation of the 'Seven GUIs' benchmark using reflex-frp.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        package =
          haskellPackages.developPackage {
            name = "seven-guis";
            root = ./.;

            source-overrides = {
              text-display = "0.0.2.0";
            };

            overrides = self: super: with pkgs.haskell.lib; {
              reanimate-svg = unmarkBroken (dontCheck super.reanimate-svg);
            };

            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with haskellPackages; [
                cabal-install
                ghcid
                haskell-language-server

                pkgs.haskellPackages.cabal-fmt
                pkgs.nixpkgs-fmt
              ]);
          };
      in
      {
        defaultPackage = package;
        devShell = haskellPackages.shellFor {
          packages = p: [ package ];
        };
      });
}
