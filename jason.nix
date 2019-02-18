{ mkDerivation, base, containers, hspec, megaparsec_7_0_4, raw-strings-qq, stdenv }:
mkDerivation {
  pname = "jason";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec_7_0_4 ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec megaparsec_7_0_4 raw-strings-qq ];
  doHaddock = false;
  homepage = "https://github.com/appositum/jason#readme";
  license = stdenv.lib.licenses.asl20;
}
