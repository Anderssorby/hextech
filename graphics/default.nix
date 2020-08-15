{ mkDerivation, aeson, animate, ansi-terminal, array, base
, brittany, bytestring, containers, extra, flow, hlint, hpack
, hspec, key-state, lens, linear, monad-logger
, morpheus-graphql-client, mtl, random, relude, safe
, safe-exceptions, sdl2, sdl2-gfx, sdl2-image, sdl2-mixer, sdl2-ttf
, StateVar, stdenv, template-haskell, text, text-conversions, time
, vector
}:
mkDerivation {
  pname = "hextech-graphics";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson animate ansi-terminal array base brittany bytestring
    containers extra flow hlint key-state lens linear monad-logger
    morpheus-graphql-client mtl random relude safe safe-exceptions sdl2
    sdl2-gfx sdl2-image sdl2-mixer sdl2-ttf StateVar template-haskell
    text text-conversions time vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson animate ansi-terminal array base brittany bytestring
    containers extra flow hlint key-state lens linear monad-logger
    morpheus-graphql-client mtl random relude safe safe-exceptions sdl2
    sdl2-gfx sdl2-image sdl2-mixer sdl2-ttf StateVar template-haskell
    text text-conversions time vector
  ];
  testHaskellDepends = [
    aeson animate ansi-terminal array base brittany bytestring
    containers extra flow hlint hspec key-state lens linear
    monad-logger morpheus-graphql-client mtl random relude safe
    safe-exceptions sdl2 sdl2-gfx sdl2-image sdl2-mixer sdl2-ttf
    StateVar template-haskell text text-conversions time vector
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
