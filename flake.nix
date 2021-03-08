{
  inputs = {
    nixpkgs = {
      url = "nixpkgs";
      flake = true;
    };
  };

  description = "Following the journey of JadaRpg";

  outputs = { self, nixpkgs, ... }: {
    overlay = final: prev:
      let
        jadas-journey = final.haskellPackages.mkDerivation rec {
          pname = "jadas-journey";
          version = "0.1.0.0";

          isLibrary = false;
          isExecutable = true;

          executableHaskellDepends = with final.haskellPackages; [
            # build depdenencies
            final.zlib
            final.pkg-config

            # dev tools
            cabal-install
            ghcid

            # base
            base
            text
            bytestring

            # parsing
            megaparsec
            parser-combinators

            # twitter
            tweet-hs
          ];

          src = builtins.path {
            path = ./src;
            name = pname;
          };

          license = "Unlicense";
        };
      in { inherit jadas-journey; };

    packages.x86_64-linux =
      let pkgs = import nixpkgs { overlays = [ self.overlay ]; };
      in { inherit (pkgs) jadas-journey; };
  };
}
