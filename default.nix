{ mkDerivation, base, pure, pure-elm, pure-cond, pure-txt, pure-prop, stdenv
}:
mkDerivation {
  pname = "pure-transition";
  version = "0.8.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base pure pure-elm pure-cond pure-txt pure-prop
  ];
  license = stdenv.lib.licenses.bsd3;
}
