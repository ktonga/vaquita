#!/bin/bash

COMMIT=$(git rev-parse --short HEAD)
TAG="gcr.io/maximal-mason-295812/vaquita:$COMMIT"

docker build -t $TAG "$@" .

# Requires setting up docker auth
# `gcloud auth configure-docker`
docker push $TAG
