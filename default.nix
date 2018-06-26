{ mkDerivation, base, pure, pure-cond, pure-css, pure-styles
, pure-theme, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-transition";
  version = "0.7.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base pure pure-cond pure-css pure-styles pure-theme pure-txt
  ];
  executableHaskellDepends = [
    base pure pure-cond pure-css pure-styles pure-theme pure-txt
  ];
  license = stdenv.lib.licenses.bsd3;
}
