{ mkDerivation, base, blaze-html, directory, filepath, hsexif
, hspec, optparse-applicative, shakespeare, stdenv, unix
}:
mkDerivation {
  pname = "phocid";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html directory filepath hsexif optparse-applicative
    shakespeare
  ];
  testHaskellDepends = [
    base blaze-html directory filepath hsexif hspec
    optparse-applicative shakespeare unix
  ];
  description = "Generate a simple HTML site from a directory of photos";
  license = stdenv.lib.licenses.gpl2;
}
