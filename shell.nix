{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, cereal, nbt, pipes-cereal, pipes-zlib, stdenv, text, lens, time, text-show, vector, zlib }:
      mkDerivation {
        pname = "minecraft-commands";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base array cereal nbt pipes-cereal pipes-zlib text lens time text-show vector zlib ];
        homepage = "https://github.com/stepcut/minecraft-commands";
        description = "a DSL for generating minecraft commands";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
