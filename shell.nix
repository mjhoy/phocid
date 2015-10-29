{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, cabal-install,
        optparse-applicative, hsexif, shakespeare,
        hspec
   }:
      mkDerivation {
        pname = "phocid";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base
                                     optparse-applicative
                                     hsexif
                                     shakespeare
                                     hspec
                                   ];
        buildTools = [ cabal-install ];
        description = "Generate a simple HTML site from a directory of photos";
        license = stdenv.lib.licenses.gpl2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
