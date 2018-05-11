#!/bin/bash

DOCKER_IMAGE="ghcjs:lts-9.21"

echo ">>> Building the frontend using GHCJS..."
stack build --stack-yaml=client/stack.yaml 

echo ">>> Copying over GHCJS build to electron" \
cp client/.stack-work/install/x86_64-linux/lts-9.21/ghcjs-0.2.1.9009021_ghc-8.0.2/bin/auction-client-exe.jsexe/all.js electron/resources/client/

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