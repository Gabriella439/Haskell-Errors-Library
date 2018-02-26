{ mkDerivation, base, exceptions, safe, stdenv, text, transformers
, transformers-compat, unexceptionalio
}:
mkDerivation {
  pname = "errors";
  version = "2.2.4";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions safe text transformers transformers-compat
    unexceptionalio
  ];
  description = "Simplified error-handling";
  license = stdenv.lib.licenses.bsd3;
}
