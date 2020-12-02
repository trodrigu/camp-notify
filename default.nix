{ mkDerivation, aeson, base, bytestring, ghcide, http-client
, http-client-tls, http-types, monad-logger, mtl, parallel-io
, persistent, persistent-postgresql, persistent-template, scalpel
, stdenv, text, time
}:
mkDerivation {
  pname = "camp-notify";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring http-client http-client-tls http-types
    monad-logger mtl parallel-io persistent persistent-postgresql
    persistent-template scalpel text time
  ];
  executableSystemDepends = [ ghcide ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
