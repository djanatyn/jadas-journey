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
        tweet-hs = final.haskellPackages.tweet-hs.overrideAttrs(attrs: {
          patches = [ ./tweet-hs.patch ];
        });

        jadas-journey = final.haskellPackages.mkDerivation rec {
          pname = "jadas-journey";
          version = "0.1.0.0";

          isLibrary = false;
          isExecutable = true;

          executableHaskellDepends = with final.haskellPackages; [
            # build depdenencies
            final.zlib
            zlib
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
            microlens

            # testing
            tasty
            tasty-hunit
            tasty-html

            # storing tweets
            store

            # config
            dhall
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
