{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    ...
  } @ inputs: let
    haskellOverlay = pkgs: final: prev: {
      "tasty-glitter" =
        pkgs.haskell.lib.dontCheck (final.callCabal2nixWithOptions "tasty-glitter"
          (nix-filter.lib {root = self;}) "" {});
    };
    overlays = final: prev: {
      haskellPackages = prev.haskellPackages.extend (haskellOverlay final);
    };
  in
    {
      inherit haskellOverlay;
      overlays.default = overlays;
    }
    // flake-utils.lib.eachDefaultSystem
    (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlays];
      };
      hpkgs = pkgs.haskellPackages;
    in rec {
      packages = {
        default = packages.tasty-glitter;
        tasty-glitter = hpkgs.tasty-glitter;
      };
      devShells = let
        nativeBuildInputs = with hpkgs; [
          cabal-install
          ghcid
          haskell-language-server
          hpack
          fourmolu
          graphviz
        ];
        withHoogle = true;
      in {
        default =
          hpkgs.shellFor
          {
            name = "tasty-glitter-shells";
            packages = p: [p.tasty-glitter];
            doBenchmark = true;
            inherit nativeBuildInputs withHoogle;
          };
      };
    });
}
