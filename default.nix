{ mkDerivation, base, stdenv,
  optparse-applicative, hsexif, shakespeare, blaze-html
 }:
mkDerivation {
  pname = "phocid";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base optparse-applicative hsexif shakespeare blaze-html ];
  description = "Generate a simple HTML site from a directory of photos";
  license = stdenv.lib.licenses.gpl2;
}
