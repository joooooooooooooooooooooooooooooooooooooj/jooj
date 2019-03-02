{ mkDerivation, base, containers, directory, hspec, megaparsec, raw-strings-qq, scientific, stdenv }:
mkDerivation {
  pname = "jooj";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [ base containers directory megaparsec scientific ];
  testHaskellDepends = [ base directory hspec megaparsec raw-strings-qq ];
  doHaddock = false;
  homepage = "https://github.com/appositum/jooj#readme";
  license = stdenv.lib.licenses.asl20;
}
