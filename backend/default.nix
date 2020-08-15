{ mkDerivation, aeson, base, bcrypt, bytestring, containers, envy
, exceptions, extra, hpack, http-types, jwt, load-env
, monad-control, morpheus-graphql, morpheus-graphql-core, mtl
, opaleye, postgresql-simple, postgresql-simple-url
, product-profunctors, resource-pool, scientific, scotty, stdenv
, stm, text, time, transformers, transformers-base
, unordered-containers, wai-cors
}:
mkDerivation {
  pname = "hextech-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bcrypt bytestring containers envy exceptions extra
    http-types jwt load-env monad-control morpheus-graphql
    morpheus-graphql-core mtl opaleye postgresql-simple
    postgresql-simple-url product-profunctors resource-pool scientific
    scotty stm text time transformers transformers-base
    unordered-containers wai-cors
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bcrypt bytestring containers envy exceptions extra
    http-types jwt load-env monad-control morpheus-graphql
    morpheus-graphql-core mtl opaleye postgresql-simple
    postgresql-simple-url product-profunctors resource-pool scientific
    scotty stm text time transformers transformers-base
    unordered-containers wai-cors
  ];
  testHaskellDepends = [
    aeson base bcrypt bytestring containers envy exceptions extra
    http-types jwt load-env monad-control morpheus-graphql
    morpheus-graphql-core mtl opaleye postgresql-simple
    postgresql-simple-url product-profunctors resource-pool scientific
    scotty stm text time transformers transformers-base
    unordered-containers wai-cors
  ];
  prePatch = "hpack";
  homepage = "https://github.com/Anderssorby/hextech#readme";
  license = stdenv.lib.licenses.bsd3;
}
