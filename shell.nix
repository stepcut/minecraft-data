{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, nbt, stdenv, text, lens, zlib, time, text-show }:
      mkDerivation {
        pname = "minecraft-commands";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base array nbt text lens zlib time text-show ];
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
