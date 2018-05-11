{ mkDerivation, base, exceptions, safe, stdenv, text, transformers
, transformers-compat
}:
mkDerivation {
  pname = "errors";
  version = "2.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions safe text transformers transformers-compat
  ];
  description = "Simplified error-handling";
  license = stdenv.lib.licenses.bsd3;
}
