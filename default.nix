{ mkDerivation, stdenv, ghc, base, pure-elm, pure-lifted
}:
mkDerivation {
  pname = "pure-ribbon";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm pure-lifted ];
  homepage = "github.com/grumply/pure-ribbon";
  license = stdenv.lib.licenses.bsd3;
}
