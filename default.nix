{ mkDerivation, aeson, base, bytestring, concurrent-extra
, http-client, http-client-tls, http-types, monad-logger, mtl
, parallel-io, persistent, persistent-postgresql
, persistent-template, scalpel, stdenv, text, time
}:
mkDerivation {
  pname = "camp-notify";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring concurrent-extra http-client http-client-tls
    http-types monad-logger mtl parallel-io persistent
    persistent-postgresql persistent-template scalpel text time
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
