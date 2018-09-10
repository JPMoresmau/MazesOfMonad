{ mkDerivation, array, base, containers, directory, filepath, HUnit
, mtl, old-locale, pretty, random, regex-posix, stdenv, time
}:
mkDerivation {
  pname = "MazesOfMonad";
  version = "1.0.9";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    array base containers directory filepath HUnit mtl old-locale
    pretty random regex-posix time
  ];
  description = "Console-based Role Playing Game";
  license = stdenv.lib.licenses.bsd3;
}
