#!/bin/bash

DOCKER_IMAGE="ghcjs:lts-9.21"

echo ">>> Building the frontend using GHCJS..."
stack build --stack-yaml=client/stack.yaml 

echo ">>> Copying over GHCJS build to electron" \
cp client/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/auction-client-exe/auction-client-exe.jsexe/all.js electron/resources/client/
cp client/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/auction-client-exe/auction-client-exe.jsexe/out.js electron/resources/client/
cp client/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/auction-client-exe/auction-client-exe.jsexe/lib.js electron/resources/client/
cp client/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/auction-client-exe/auction-client-exe.jsexe/rts.js electron/resources/client/
cp client/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/auction-client-exe/auction-client-exe.jsexe/all.js.externs electron/resources/client/

echo ">>> Building the server..."
stack build --stack-yaml=server/stack.yaml \
  && cp server/.stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/auction-server-exe/auction-server-exe electron/resources/server

echo ">>> Copying over FaeServer"
docker create --name faeServer teamfae/faeserver
docker cp faeServer:/etc/faeServer.sh electron/resources/faeserver
docker rm faeServer

echo ">>> Copying over Fae contracts..."
cp -r server/contracts electron/contracts

echo ">>> Done"