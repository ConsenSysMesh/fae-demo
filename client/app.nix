{ mkDerivation, base, miso, stdenv }:
mkDerivation {
  pname = "auction-demo-client";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso ];
  description = "Auction Demo Client";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
