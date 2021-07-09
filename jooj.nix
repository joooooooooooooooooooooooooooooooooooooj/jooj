{ lib, mkDerivation, base, containers, directory, hspec, megaparsec
, QuickCheck, quickcheck-instances, raw-strings-qq, scientific
, stdenv
}:
mkDerivation {
  pname = "jooj";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [ base containers megaparsec scientific ];
  testHaskellDepends = [ base directory hspec megaparsec QuickCheck quickcheck-instances raw-strings-qq scientific ];
  doHaddock = false;
  homepage = "https://github.com/appositum/jooj#readme";
  license = lib.licenses.asl20;
}
