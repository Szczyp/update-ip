{ nixpkgs ? import "/home/qb/projects/nixpkgs" {}, compiler ? "ghc7102" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring, filepath
      , classy-prelude, either, errors, http-client, http-types, lens
      , lens-aeson, pipes-attoparsec, pipes-http, stdenv, transformers
      , cabal-install, ghc-mod, stylish-haskell, hoogle, hasktags, hlint
      }:
      mkDerivation {
        pname = "update-ip";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          aeson attoparsec base bytestring classy-prelude either errors
          http-client http-types lens lens-aeson pipes-attoparsec pipes-http
          transformers filepath
        ];
        buildTools = [ cabal-install ghc-mod stylish-haskell hoogle hasktags hlint ];
        homepage = "http://github.com/Szczyp/update-ip";
        description = "Updates a dns record on name.com if public ip changed";
        license = stdenv.lib.licenses.gpl3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
