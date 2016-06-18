with (import <nixpkgs> {});
with haskell.packages.ghc801;
(mkDerivation {
  pname = "update-ip";
  version = "0.3.0.0";
  src = ./.;
  buildDepends = [ aeson attoparsec base case-insensitive classy-prelude filepath
                   lens lens-aeson lifted-async monad-control mtl transformers-base wreq ];
  buildTools = [ cabal-install ghc-mod stylish-haskell hoogle hlint ];
  license = stdenv.lib.licenses.gpl3;
}).env
