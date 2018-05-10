#!/bin/bash

DOCKER_IMAGE="ghcjs:lts-9.21"

mkdir -p build/bin
mkdir -p build/static

echo ">>> Building the frontend..."
# Build the frontend and copy over the javascript to the build dir
stack build --stack-yaml=client/stack.yaml 
echo ">>> Copying over all.js" \
cp client/.stack-work/install/x86_64-linux/lts-9.21/ghcjs-0.2.1.9009021_ghc-8.0.2/bin/auction-client-exe.jsexe/all.js build/static/all.js

echo ">>> Building the server..."
# Build the backend, and copy over the server to the build dir
stack build --stack-yaml=server/stack.yaml \
  && cp $(stack path --stack-yaml=server/stack.yaml --local-install-root)/bin/server build/bin/server

# copy over static css and img static assets from client to build dir

echo ">>> Done"