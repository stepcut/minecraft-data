{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bytestring, cereal, containers
      , lens, nbt, pipes, pipes-bytestring, pipes-cereal, pipes-parse
      , pipes-zlib, stdenv, text, text-show, time, vector, zlib, cabal-install, mtl
      }:
      mkDerivation {
        pname = "minecraft-data";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          array base bytestring cereal containers lens nbt pipes
          pipes-bytestring pipes-cereal pipes-parse pipes-zlib text text-show
          time vector zlib mtl
        ];
        buildTools = [ cabal-install ];
        homepage = "https://github.com/stepcut/minecraft-data";
        description = "a DSL for generating minecraft commands and levels";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
