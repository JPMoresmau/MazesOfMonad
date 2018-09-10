with import <nixpkgs> { };
let
  monadMaze = (haskellPackages.callPackage ./mmaze.nix {});
in stdenv.mkDerivation {
  name = "maze-monad-env";
  buildInputs = [
    monadMaze
  ];
}
