with (import <nixpkgs> {});
with haskell.packages.ghc7102;
(mkDerivation {
  pname = "update-ip";
  version = "0.3.0.0";
  src = ./.;
  buildDepends = [ aeson attoparsec base case-insensitive classy-prelude filepath
                   lens lens-aeson monad-control mtl transformers-base wreq ];
  buildTools = [ cabal-install ghc-mod stylish-haskell hoogle hasktags hlint ];
  license = stdenv.lib.licenses.gpl3;
}).env
