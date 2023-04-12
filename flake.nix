{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-explorer.url = "github:LightAndLight/git-explorer";
  };
  outputs = { self, nixpkgs, flake-utils, git-explorer }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "927";
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            git-explorer.defaultPackage.${system}
            
            haskell.packages."ghc${ghcVersion}".ghc
            cabal-install
            (haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
            
            # For the `hasql` Haskell package
            postgresql
          ];
        };
      }
    );
}
